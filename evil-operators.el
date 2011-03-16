;;;; Operator-Pending state

(require 'evil-vars)
(require 'evil-common)
(require 'evil-states)
(require 'evil-types)
(require 'evil-motions)
(require 'evil-compatibility)

(evil-define-state operator
  "Operator-Pending state"
  :tag " <O> "
  :cursor evil-half-cursor
  :enable (evil-operator-shortcut-map operator normal))

(evil-define-keymap evil-operator-shortcut-map
  "Keymap for Operator-Pending shortcuts like \"dd\" and \"gqq\"."
  :local t
  (setq evil-operator-shortcut-map (make-sparse-keymap))
  (evil-refresh-local-maps))

;; the half-height "Operator-Pending cursor" cannot be specified
;; as a static `cursor-type' value, since its height depends on
;; the current font size
(defun evil-half-cursor ()
  "Change cursor to a half-height box.
\(This is really just a thick horizontal bar.)"
  (let (height)
    ;; make `window-line-height' reliable
    (redisplay)
    (setq height (window-line-height))
    (setq height (+ (nth 0 height) (nth 3 height)))
    ;; cut cursor height in half
    (setq height (/ height 2))
    (setq cursor-type (cons 'hbar height))
    ;; ensure the cursor is redisplayed
    (force-window-update (selected-window))
    (redisplay)))

(defmacro evil-define-operator (operator args &rest body)
  "Define an operator command OPERATOR.
ARGS is the argument list, which must contain at least two
arguments: the beginning and end of the range."
  (declare (indent defun)
           (debug (&define name lambda-list
                           [&optional stringp]
                           [&rest keywordp sexp]
                           def-body)))
  (let ((repeat t)
        beg end interactive keep-visual keyword type whole-lines)
    ;; collect BEG, END and TYPE
    (setq args (delq '&optional args)
          beg (or (pop args) 'beg)
          end (or (pop args) 'end)
          type (or (pop args) 'type))
    ;; collect docstring
    (when (stringp (car body))
      (setq doc (pop body)))
    ;; collect keywords
    (while (keywordp (car-safe body))
      (setq keyword (pop body))
      (cond
       ((eq keyword :keep-visual)
        (setq keep-visual (pop body)))
       ((eq keyword :whole-lines)
        (setq whole-lines (pop body)))
       ((eq keyword :repeat)
        (setq repeat (pop body)))
       (t
        (pop body))))
    ;; collect `interactive' specification
    (when (eq (car-safe (car-safe body)) 'interactive)
      (setq interactive (cdr (pop body))))
    ;; macro expansion
    `(progn
       (add-to-list 'evil-operators ',operator t)
       (defun ,operator (,beg ,end &optional ,type ,@args)
         ,@(when doc `(,doc))
         (interactive
          (append (evil-operator-range) ,@interactive))
         (if (and evil-inhibit-operator
                  (called-interactively-p))
             (setq evil-inhibit-operator nil)
           ,@body)))))

(defun evil-operator-range (&optional type)
  "Read a motion from the keyboard and return its buffer positions.
The return value is a list (BEG END), which can be used
in the `interactive' specification of an operator command."
  (let (beg end motion range)
    (evil-save-echo-area
      (evil-save-state
        (cond
         ((or (evil-visual-state-p)
              (region-active-p))
          (list (region-beginning) (region-end)))
         (t
          (evil-operator-state)
          (define-key evil-operator-shortcut-map
            (vector last-command-event) 'evil-line)
          (let ((count-and-cmd (evil-extract-count (this-command-keys))))
            (define-key evil-operator-shortcut-map
              (nth 2 count-and-cmd) 'evil-line))
          (setq motion (evil-keypress-parser)
                evil-this-motion (pop motion)
                evil-this-motion-count (pop motion)
                evil-this-type nil)
          (cond
           (evil-repeat-count
            (setq evil-this-motion-count evil-repeat-count
                  ;; only the first operator's count is overwritten
                  evil-repeat-count nil))
           ((or current-prefix-arg evil-this-motion-count)
            (setq evil-this-motion-count
                  (* (prefix-numeric-value current-prefix-arg)
                     (prefix-numeric-value evil-this-motion-count)))))
          (cond
           ((or (null evil-this-motion)
                (eq evil-this-motion 'keyboard-quit))
            (setq quit-flag t))
           (t
            (setq range (evil-motion-range
                         evil-this-motion
                         evil-this-motion-count
                         (or type
                             (evil-type evil-this-motion)
                             'exclusive))
                  beg (pop range)
                  end (pop range)
                  evil-this-type (pop range))
            (list beg end)))))))))

(defun evil-motion-range (motion &optional count type)
  "Execute a motion and return the buffer positions.
The return value is a list (BEG END TYPE)."
  (evil-save-region
    (transient-mark-mode 1)
    (setq evil-motion-marker (move-marker (make-marker) (point)))
    (unwind-protect
        (let ((current-prefix-arg count)
              (evil-this-type type))
          (condition-case err
              (call-interactively motion)
            (error (prog1 nil
                     (setq evil-write-echo-area t)
                     (message (error-message-string err)))))
          (cond
           ;; if text has been selected (i.e., it's a text object),
           ;; return the selection
           ((or (evil-visual-state-p)
                (region-active-p))
            (evil-expand (region-beginning) (region-end) evil-this-type))
           (t
            (evil-expand evil-motion-marker (point) evil-this-type))))
      ;; delete marker so it doesn't slow down editing
      (move-marker evil-motion-marker nil)
      (setq evil-motion-marker nil))))

(defun evil-keypress-parser (&optional input)
  "Read from keyboard or INPUT and build a command description.
Returns (CMD COUNT), where COUNT is the numeric prefix argument.
Both COUNT and CMD may be nil."
  (let ((input (append input nil))
        (inhibit-quit t)
        char cmd count digit seq)
    (while (progn
             (setq char (or (pop input) (read-event)))
             (when (symbolp char)
               (setq char (or (get char 'ascii-character) char)))
             ;; this trick from simple.el's `digit-argument'
             ;; converts keystrokes like C-0 and C-M-1 to digits
             (if (or (characterp char) (integerp char))
                 (setq digit (- (logand char ?\177) ?0))
               (setq digit nil))
             (if (keymapp cmd)
                 (setq seq (append seq (list char)))
               (setq seq (list char)))
             (setq cmd (key-binding (vconcat seq) t))
             (cond
              ;; if CMD is a keymap, we need to read more
              ((keymapp cmd)
               t)
              ;; numeric prefix argument
              ((or (memq cmd '(digit-argument))
                   (and (eq (length seq) 1)
                        (not (keymapp cmd))
                        count
                        (memq digit '(0 1 2 3 4 5 6 7 8 9))))
               ;; store digits in a string, which is easily converted
               ;; to a number afterwards
               (setq count (concat (or count "")
                                   (number-to-string digit)))
               t)
              ;; catch middle digits like "da2w"
              ((and (not cmd)
                    (> (length seq) 1)
                    (memq digit '(0 1 2 3 4 5 6 7 8 9)))
               (setq count (concat (or count "")
                                   (number-to-string digit)))
               ;; remove the digit from the key sequence
               ;; so we can see if the previous one goes anywhere
               (setq seq (nbutlast seq 1))
               (setq cmd (key-binding (vconcat seq)))
               t)
              ((eq cmd 'negative-argument)
               (unless count
                 (setq count "-")))
              ;; user pressed C-g, so return nil for CMD
              ((eq cmd 'keyboard-quit)
               (setq cmd nil))
              ;; we are done, exit the `while' loop
              (t
               nil))))
    ;; determine COUNT
    (when (stringp count)
      (if (string= count "-")
          (setq count nil)
        (setq count (string-to-number count))))
    ;; return command description
    (list cmd count)))

;;; Operator commands

(evil-define-operator evil-delete (beg end type register)
  "Delete text."
  (kill-region beg end))

(evil-define-operator evil-rot13 (beg end)
  "ROT13 encrypt text."
  (rot13-region beg end))


(evil-define-operator evil-yank (begin end type register)
  "Saves the characters in motion into the kill-ring."
  ;; TODO: this is a hack as long as the `type' parameter does not
  ;; work
  (setq type evil-this-type)
  (cond
   ((eq type 'block)
    (evil-yank-rectangle begin end register))
   ((eq type 'line)
    (evil-yank-lines begin end register))
   (t
    (let ((text (buffer-substring begin end)))
      (if register
          (set-register register text)
        (kill-new text))))))

(defun evil-yank-lines (begin end register)
  "Saves the next count lines into the kill-ring."
  (let ((txt (buffer-substring begin end)))
    ;; Ensure the text ends with newline.  This is required if the
    ;; deleted lines were the last lines in the buffer.
    (unless (= (aref txt (1- (length txt))) ?\n)
      (setq txt (concat txt "\n")))
    (if register
        (progn
          (put-text-property 0 (length txt)
                             'yank-handler
                             (list #'evil-yank-line-handler txt)
                             txt)
          (set-register register txt))
      (kill-new txt nil (list #'evil-yank-line-handler txt)))))

(defun evil-yank-rectangle (begin end register)
  "Stores the rectangle defined by motion into the kill-ring."
  ;; TODO: yanking should not insert spaces or expand tabs.
  (let ((begrow (line-number-at-pos begin))
        (begcol (save-excursion (goto-char begin) (current-column)))
        (endrow (line-number-at-pos end))
        (endcol (save-excursion (goto-char end) (current-column)))
        (parts nil))
    (when (> begrow endrow) (evil-swap begrow endrow))
    (when (> begcol endcol) (evil-swap begcol endcol))

    (goto-line endrow)
    (dotimes (i (1+ (- endrow begrow)))
      (let ((beg (save-excursion (move-to-column begcol) (point)))
            (end (save-excursion (move-to-column endcol) (point))))
        (push (cons (save-excursion
                      (goto-char beg)
                      (- (current-column) begcol))
                    (buffer-substring beg end))
              parts)
        (forward-line -1)))
    (let* ((txt (mapconcat #'cdr parts "\n"))
           ;; `txt' contains the block as single lines
           (yinfo (list #'evil-yank-block-handler
                       (cons (- endcol begcol) parts)
                       nil
                       #'delete-rectangle)))
      (if register
          (progn
            (put-text-property 0 (length txt) 'yank-handler yinfo txt)
            (set-register register txt))
        (kill-new txt nil yinfo)))
    (goto-line begrow)
    (move-to-column begcol)))


(defun evil-yank-line-handler (text)
  "Inserts the current text linewise."
  (beginning-of-line)
  (set-mark (point))
  (insert text))


(defun evil-yank-block-handler (text)
  "Inserts the current text as block."
  ;; TODO: yank-pop with count will not work for blocks, because
  ;; it's difficult to place (point) (or (mark)) at the correct
  ;; position since they may not exist.
  (let ((ncols (car text))
        (parts (cdr text))
        (col (current-column))
        (current-line (line-number-at-pos (point)))
        (last-pos (point)))

    (set-mark (point))
    (dolist (part parts)

      (let* ((offset (car part))
             (txt (cdr part))
             (len (length txt)))

        ;; maybe we have to insert a new line at eob
        (when (< (line-number-at-pos (point))
                 current-line)
          (goto-char (point-max))
          (newline))
        (setq current-line (1+ current-line))

        (unless (and (< (current-column) col)   ; nothing in this line
                     (<= offset 0) (zerop len)) ; and nothing to insert
          (move-to-column (+ col (max 0 offset)) t)
          (insert txt)
          (unless (eolp)
            ;; text follows, so we have to insert spaces
            (insert (make-string (- ncols len) ? ))))
        (setq last-pos (point))
        (forward-line 1)))
    (goto-char last-pos)
    (exchange-point-and-mark)))


(defun evil-paste-before (count &optional register)
  "Pastes the latest yanked text before the cursor position."
  (interactive "P")
  (let ((pos (point))
        beg end)
    (save-excursion
      (dotimes (i (or count 1))
        (if register
            (insert-for-yank (get-register register))
          (set-mark (point))
          (insert-for-yank (current-kill 0))
          (setq beg (min (point) (mark t) (or beg (point)))
                end (max (point) (mark t) (or end (point)))))))
    (let* ((txt (if register (get-register register) (current-kill 0)))
           (yhandler (get-text-property 0 'yank-handler txt)))
      (when (eq (car-safe yhandler) 'evil-yank-line-handler)
        ;; place cursor at for non-blank of first inserted line
        (goto-char pos)
        (evil-first-non-blank)))))


(defun evil-paste-behind (count &optional register)
  "Pastes the latest yanked text behind point."
  (interactive "P")
  (let ((txt (if register (get-register register) (current-kill 0))))
    (unless txt
      (error "Kill-ring empty"))
    (let ((yhandler (get-text-property 0 'yank-handler txt))
          (pos (point)))
      (cond
       ((eq (car-safe yhandler) 'evil-yank-line-handler)
        (let ((at-eob (= (line-end-position) (point-max))))
          (forward-line)
          (when at-eob (newline))
          (evil-paste-before count register)
          (evil-first-non-blank)))

       ((eq (car-safe yhandler) 'evil-yank-block-handler)
        (forward-char)
        (evil-paste-before count register))

       (t
        (unless (eobp) (forward-char))
        (evil-paste-before count register))))))

(provide 'evil-operators)

;;; evil-operators.el ends here
