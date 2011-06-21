;;;; Operator-Pending state

(require 'evil-states)
(require 'evil-visual)
(require 'evil-insert)

(require 'rect)

(evil-define-state operator
  "Operator-Pending state"
  :tag " <O> "
  :cursor evil-half-cursor
  :enable (evil-operator-shortcut-map operator motion normal))

(evil-define-keymap evil-operator-shortcut-map
  "Keymap for Operator-Pending shortcuts like \"dd\" and \"gqq\"."
  :local t
  (setq evil-operator-shortcut-map (make-sparse-keymap))
  (evil-refresh-local-keymaps))

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

\(fn OPERATOR (BEG END ARGS...) DOC [[KEY VALUE]...] BODY...)"
  (declare (indent defun)
           (debug (&define name lambda-list
                           [&optional stringp]
                           [&rest keywordp sexp]
                           [&optional ("interactive" interactive)]
                           def-body)))
  (let ((move-point t)
        (keep-visual nil)
        (whole-lines nil)
        (motion nil)
        arg doc beg end interactive key keys type)
    ;; collect BEG, END and TYPE
    (setq args (delq '&optional args)
          beg (or (pop args) 'beg)
          end (or (pop args) 'end)
          type (or (pop args) 'type))
    ;; collect docstring
    (when (and (> (length body) 1)
               (or (eq (car-safe (car-safe body)) 'format)
                   (stringp (car-safe body))))
      (setq doc (pop body)))
    ;; collect keywords
    (while (keywordp (car-safe body))
      (setq key (pop body)
            arg (pop body))
      (cond
       ((eq key :motion)
        (setq motion arg)
        (unless motion
          (setq motion 'undefined)))
       ((eq key :keep-visual)
        (setq keep-visual arg))
       ((eq key :move-point)
        (setq move-point arg))
       ((eq key :whole-lines)
        (setq whole-lines arg))
       (t
        (setq keys (append keys (list key arg))))))
    ;; collect `interactive' specification
    (when (eq (car-safe (car-safe body)) 'interactive)
      (setq interactive (cdr (pop body))))
    ;; macro expansion
    `(evil-define-command ,operator (,beg ,end &optional ,type ,@args)
       ,@(when doc `(,doc))
       ,@keys
       :exclude-newline t
       :keep-visual t
       (interactive
        (let* ((orig (point))
               (beg orig)
               (end orig)
               range type)
          (unwind-protect
              (setq evil-this-operator this-command
                    range (evil-operator-range ',motion)
                    beg (evil-range-beginning range)
                    end (evil-range-end range)
                    type (evil-type range)
                    range (append (evil-range beg end type)
                                  (progn ,@interactive)))
            (setq orig (point))
            (when ,move-point
              (if (and ',motion (not (evil-visual-state-p)))
                  (goto-char (max beg (1- end)))
                (if (eq type 'block)
                    (evil-visual-block-rotate 'upper-left beg end)
                  (goto-char beg))))
            (if ,keep-visual
                (when (evil-visual-state-p)
                  (evil-visual-expand-region))
              (when (evil-visual-state-p)
                (evil-normal-state))
              (when (region-active-p)
                (evil-active-region -1)))
            (unless ,move-point
              (goto-char orig)))
          range))
       (if (and evil-inhibit-operator
                (evil-called-interactively-p))
           (setq evil-inhibit-operator nil)
         ,@body))))

(defun evil-operator-range (&optional motion)
  "Read a motion from the keyboard and return its buffer positions.
The return value is a list (BEG END TYPE), which can be used
in the `interactive' specification of an operator command."
  (let ((beg (point)) (end (point))
        command count modifier range type)
    (evil-save-echo-area
      (cond
       ;; Visual selection
       ((evil-visual-state-p)
        (setq beg (evil-visual-beginning)
              end (evil-visual-end)
              type (evil-visual-type)))
       ((region-active-p)
        (setq beg (region-beginning)
              end (region-end)
              type (or evil-this-type 'exclusive)))
       (t
        ;; read motion from keyboard
        (evil-save-state
          (unless motion
            (evil-operator-state)
            ;; Make linewise operator shortcuts. E.g., "d" yields the
            ;; shortcut "dd", and "g?" yields shortcuts "g??" and "g?g?".
            (let ((keys (nth 2 (evil-extract-count (this-command-keys)))))
              (setq keys (listify-key-sequence keys))
              (dotimes (var (length keys))
                (define-key evil-operator-shortcut-map
                  (vconcat (nthcdr var keys)) 'evil-line)))
            (setq command (evil-read-motion motion)
                  motion (pop command)
                  count (pop command)
                  type (pop command)))
          (cond
           ((null motion)
            (setq quit-flag t))
           ((eq motion 'undefined)
            (setq motion nil))
           (evil-repeat-count
            (setq count evil-repeat-count
                  ;; only the first operator's count is overwritten
                  evil-repeat-count nil))
           ((or count current-prefix-arg)
            ;; multiply operator count and motion count together
            (setq count
                  (* (prefix-numeric-value count)
                     (prefix-numeric-value current-prefix-arg)))))
          (when motion
            (evil-with-state operator
              (setq range (evil-motion-range
                           motion
                           count
                           type)
                    beg (pop range)
                    end (pop range)
                    type (pop range))))
          ;; update global variables
          (setq evil-this-motion motion
                evil-this-motion-count count
                evil-this-type type))))
      (evil-range beg end type))))

(defun evil-motion-range (motion &optional count type)
  "Execute a motion and return the buffer positions.
The return value is a list (BEG END TYPE)."
  (evil-save-region
    (evil-transient-mark 1)
    (setq evil-motion-marker (move-marker (make-marker) (point))
          type (or type (evil-type motion 'exclusive)))
    (unwind-protect
        (let ((current-prefix-arg count)
              ;; Store the type in global variable `evil-this-type'.
              ;; Motions can change their type during execution
              ;; by setting this variable.
              (evil-this-type type))
          (condition-case err
              (call-interactively motion)
            (error (prog1 nil
                     (setq evil-this-type 'exclusive
                           evil-write-echo-area t)
                     (message (error-message-string err)))))
          (cond
           ;; the motion made a Visual selection: use that
           ((evil-visual-state-p)
            (evil-range (evil-visual-beginning) (evil-visual-end)
                        (evil-visual-type)))
           ;; the motion made an active region
           ((region-active-p)
            (evil-range (region-beginning) (region-end)
                        evil-this-type))
           ;; default case: use range from previous position
           ;; to current position
           (t
            (apply 'evil-expand
                   (evil-normalize
                    evil-motion-marker (point) evil-this-type)))))
      ;; delete marker so it doesn't slow down editing
      (move-marker evil-motion-marker nil)
      (setq evil-motion-marker nil))))

(defun evil-read-motion (&optional motion count type modifier)
  "Read a MOTION, motion COUNT and motion TYPE from the keyboard.
The type may be overridden with MODIFIER, which may be a type
or a Visual selection as defined by `evil-define-visual-selection'.
Return a list (MOTION COUNT TYPE)."
  (let ((modifiers '((evil-visual-char . char)
                     (evil-visual-line . line)
                     (evil-visual-block . block)))
        command prefix)
    (unless motion
      (while (progn
               (setq command (evil-keypress-parser)
                     motion (pop command)
                     prefix (pop command))
               (when prefix
                 (if count
                     (setq count (string-to-number
                                  (concat (number-to-string count)
                                          (number-to-string prefix))))
                   (setq count prefix)))
               ;; if the command is a type modifier, read more
               (when (rassq motion evil-visual-alist)
                 (setq modifier
                       (or modifier
                           (car (rassq motion evil-visual-alist))))))))
    (setq type (or type (evil-type motion 'exclusive)))
    (when modifier
      (cond
       ((eq modifier 'char)
        ;; TODO: this behavior could be less hard-coded
        (if (eq type 'exclusive)
            (setq type 'inclusive)
          (setq type 'exclusive)))
       (t
        (setq type modifier))))
    (list motion count type)))

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
              ((memq cmd '(keyboard-quit undefined))
               (setq cmd nil)))))
    ;; determine COUNT
    (when (stringp count)
      (if (string= count "-")
          (setq count nil)
        (setq count (string-to-number count))))
    ;; return command description
    (list cmd count)))

;;; Operator commands

(evil-define-operator evil-rot13 (beg end)
  "ROT13 encrypt text."
  (rot13-region beg end))

(evil-define-operator evil-yank (begin end type register)
  "Saves the characters in motion into the kill-ring."
  :move-point nil
  (cond
   ((eq type 'block)
    (evil-yank-rectangle begin end register))
   ((eq type 'line)
    (evil-yank-lines begin end register))
   (t
    (evil-yank-characters begin end register))))

(defun evil-yank-characters (begin end register)
  "Saves the characters defined by the region BEGIN and END in the kill-ring."
  (let ((text (buffer-substring begin end)))
    (if register
        (set-register register text)
      (kill-new text))))

(defun evil-yank-lines (begin end register)
  "Saves the lines in the region BEGIN and END into the kill-ring."
  (let ((txt (buffer-substring begin end))
        (yinfo (list #'evil-yank-line-handler)))
    ;; Ensure the text ends with newline. This is required if the
    ;; deleted lines were the last lines in the buffer.
    (when (or (zerop (length txt))
              (/= (aref txt (1- (length txt))) ?\n))
      (setq txt (concat txt "\n")))
    (setq txt (propertize txt 'yank-handler yinfo))
    (if register
        (set-register register txt)
      (kill-new txt))))

(defun evil-yank-rectangle (begin end register)
  "Stores the rectangle defined by region BEGIN and END into the kill-ring."
  (let ((lines (list nil)))
    (apply-on-rectangle #'extract-rectangle-line begin end lines)
    ;; We remove spaces from the beginning and the end of the next.
    ;; Spaces are inserted explicitly in the yank-handler in order to
    ;; NOT insert lines full of spaces.
    (setq lines (nreverse (cdr lines)))
    ;; `txt' is used as default insert text when pasting this rectangle
    ;; in another program, e.g., using the X clipboard.
    (let* ((yinfo (list #'evil-yank-block-handler
                        lines
                        nil
                        #'evil-delete-yanked-rectangle))
           (txt (propertize (mapconcat #'identity lines "\n")
                            'yank-handler yinfo)))
      (if register
          (set-register register txt)
        (kill-new txt)))))

(defun evil-yank-line-handler (text)
  "Inserts the current text linewise."
  (let ((text (apply #'concat (make-list (or evil-paste-count 1) text)))
        (opoint (point)))
    (remove-list-of-text-properties
     0 (length text) yank-excluded-properties text)
    (cond
     ((eq this-command 'evil-paste-behind)
      (end-of-line)
      (set-mark (point))
      (newline)
      (insert text)
      (delete-char -1) ; delete the last newline
      (setq evil-last-paste
            (list 'evil-paste-behind
                  evil-paste-count
                  opoint
                  (mark t)
                  (point)))
      (set-mark (1+ (mark t))))
     (t
      (beginning-of-line)
      (set-mark (point))
      (insert text)
      (setq evil-last-paste
            (list 'evil-paste-before
                  evil-paste-count
                  opoint
                  (mark t)
                  (point)))))
    (exchange-point-and-mark)
    (back-to-indentation)))

(defun evil-yank-block-handler (lines)
  "Inserts the current text as block."
  (let ((count (or evil-paste-count 1))
        (col (if (eq this-command 'evil-paste-behind)
                 (1+ (current-column))
               (current-column)))
        (current-line (line-number-at-pos (point)))
        (opoint (point)))

    (dolist (line lines)
      ;; concat multiple copies according to count
      (setq line (apply #'concat (make-list count line)))
      ;; strip whitespaces at beginning and end
      (string-match "^ *\\(.*?\\) *$" line)
      (let ((txt (match-string 1 line))
            (begextra (match-beginning 1))
            (endextra (- (match-end 0) (match-end 1))))
        ;; maybe we have to insert a new line at eob
        (while (< (line-number-at-pos (point))
                  current-line)
          (goto-char (point-max))
          (newline))
        (setq current-line (1+ current-line))
        ;; insert text unless we insert an empty line behind eol
        (unless (and (< (save-excursion
                          (goto-char (line-end-position))
                          (current-column))
                        col)               ; nothing in this line
                     (zerop (length txt))) ; and nothing to insert
          ;; if we paste behind eol, it may be sufficient to insert tabs
          (if (< (save-excursion
                   (goto-char (line-end-position))
                   (current-column))
                 col)
              (move-to-column (+ col begextra) t)
            (move-to-column col t)
            (insert (make-string begextra ? )))
          (remove-list-of-text-properties 0 (length txt)
                                          yank-excluded-properties txt)
          (insert txt)
          (unless (eolp)
            ;; text follows, so we have to insert spaces
            (insert (make-string endextra ? ))))
        (forward-line 1)))
    (setq evil-last-paste
          (list this-command
                evil-paste-count
                opoint
                (length lines)                   ; number of rows
                (* count (length (car lines))))) ; number of colums
    (goto-char opoint)
    (when (and (eq this-command 'evil-paste-behind)
               (not (eolp)))
      (forward-char))))

(defun evil-delete-yanked-rectangle (nrows ncols)
  "Special function to delete the block yanked by a previous paste command."
  (let ((opoint (point))
        (col (if (eq last-command 'evil-paste-behind)
                 (1+ (current-column))
               (current-column))))
    (dotimes (i nrows)
      (delete-region (save-excursion
                       (move-to-column col)
                       (point))
                     (save-excursion
                       (move-to-column (+ col ncols))
                       (point)))
      (unless (eobp) (forward-line)))
    (goto-char opoint)))

(defun evil-paste-before (count &optional register)
  "Pastes the latest yanked text before the cursor position."
  (interactive "P")
  (evil-with-undo
    (let* ((txt (if register (get-register register) (current-kill 0)))
           (yhandler (car-safe (get-text-property 0 'yank-handler txt))))
      (if (memq yhandler '(evil-yank-line-handler evil-yank-block-handler))
          (let ((evil-paste-count count)
                (this-command 'evil-paste-before)) ; for non-interactive use
            (insert-for-yank txt))
        ;; no yank-handler, default
        (let ((opoint (point)))
          (dotimes (i (or count 1))
            (insert-for-yank txt))
          (set-mark opoint)
          (setq evil-last-paste
                (list 'evil-paste-before
                      count
                      opoint
                      opoint    ; begin
                      (point))) ; end
          (exchange-point-and-mark)))
      ;; no paste pop after pasting a register
      (when register
        (setq evil-last-paste nil)))))

(defun evil-paste-behind (count &optional register)
  "Pastes the latest yanked text behind point."
  (interactive "P")
  (evil-with-undo
    (let* ((txt (if register (get-register register) (current-kill 0)))
           (yhandler (car-safe (get-text-property 0 'yank-handler txt))))
      (if (memq yhandler '(evil-yank-line-handler evil-yank-block-handler))
          (let ((evil-paste-count count)
                (this-command 'evil-paste-behind)) ; for non-interactive use
            (insert-for-yank txt))
        ;; no yank-handler, default
        (let ((opoint (point)))
          ;; TODO: Perhaps it is better to collect a list of all
          ;; (point . mark) pairs to undo the yanking for count > 1.
          ;; The reason is that this yanking could very well use
          ;; `yank-handler'.
          (unless (eolp) (forward-char))
          (let ((begin (point)))
            (dotimes (i (or count 1))
              (insert-for-yank txt))
            (setq evil-last-paste
                  (list 'evil-paste-behind
                        count
                        opoint
                        begin     ; begin
                        (point))) ; end
            (backward-char))))
      (when register
        (setq evil-last-paste nil)))))

;; TODO: if undoing is disabled in the current buffer paste pop won't
;; work. Although this is probably not a big problem because usually
;; buffers for editing where `evil-paste-pop' may be useful have
;; undoing enabled. A solution would be to temporarily enable undo
;; when pasting and storing the undo-information in a special variable
;; that does not interfere with buffer-undo-list
(defun evil-paste-pop (count)
  "Replace the just-yanked stretch of killed text with a different stretch.
This command is allowed only immediatly after a `yank',
`evil-paste-before', `evil-paste-behind' or `evil-paste-pop'.
This command uses the same paste command as before, i.e., when
used after `evil-paste-behind' the new text is also yanked using
`evil-paste-behind', used with the same paste-count argument.

The COUNT argument inserts the COUNTth previous kill.  If COUNT
is negative this is a more recent kill."
  (interactive "p")
  (unless (memq last-command
                '(evil-paste-behind
                  evil-paste-before))
    (error "Previous command was not an evil-paste: %s" last-command))
  (unless evil-last-paste
    (error "Previous paste command used a register"))
  (evil-undo-pop)
  (goto-char (nth 2 evil-last-paste))
  (current-kill count)
  (setq this-command (nth 0 evil-last-paste))
  (funcall (nth 0 evil-last-paste) (nth 1 evil-last-paste)))

(defun evil-paste-pop-next (count)
  "Same as `evil-paste-pop' but with negative argument."
  (interactive "p")
  (evil-paste-pop (- count)))

(evil-define-operator evil-delete (beg end type register)
  "Delete and save in kill-ring or REGISTER."
  (evil-yank beg end type register)
  (cond
   ((eq type 'block)
    (delete-rectangle beg end))
   ((and (eq type 'line)
         (= (point-max) end)
         (/= (point-min) beg))
    (delete-region (1- beg) end))
   (t
    (delete-region beg end))))

(evil-define-operator evil-change (beg end type register)
  "Delete region and change to insert state.
If the region is linewise insertion starts on an empty line.
If region is a block, the inserted text in inserted at each line
of the block."
  (let ((nlines (1+ (- (line-number-at-pos end)
                       (line-number-at-pos beg))))
        (at-eob (= (point-max) end)))
    (evil-delete beg end type register)
    (cond
     ((eq type 'line)
      (if at-eob
          (evil-insert-below 1)
        (evil-insert-above 1)))
     ((eq type 'block)
      (evil-insert-before 1 nlines))
     (t
      (evil-insert-before 1)))))

(evil-define-operator evil-delete-char (beg end type)
  "Delete next character."
  :motion evil-forward-char
  (evil-delete beg end type))

(evil-define-operator evil-delete-backward-char (beg end type)
  "Delete previous character."
  :motion evil-backward-char
  (evil-delete beg end type))

(evil-define-operator evil-join-lines (beg end)
  "Join lines from BEG to END, with a minimum of two lines."
  (goto-char beg)
  (evil-join-successive-lines
   (1+ (- (line-number-at-pos end)
          (line-number-at-pos)))))

(defun evil-join-successive-lines (count)
  "Join COUNT lines with a minimum of two lines."
  (interactive "p")
  (join-line 1))

;; TODO: register
(defun evil-change-chars (count &optional register)
  "Remove the next COUNT characters and switch to insert-state."
  (interactive "p")
  (delete-region (point)
                 (min (+ (point) count)
                      (line-end-position)))
  (evil-insert-before 1))

(provide 'evil-operators)

;;; evil-operators.el ends here
