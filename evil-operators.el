;;;; Operator-Pending state

(require 'evil-states)
(require 'evil-types)

(evil-define-keymap evil-operator-shortcut-map
  "Keymap for Operator-Pending shortcuts like \"dd\" and \"gqq\"."
  :local t
  (setq evil-operator-shortcut-map (make-sparse-keymap))
  (evil-refresh-local-maps))

(evil-define-state operator
  "Operator-Pending state"
  :tag " <O> "
  :cursor evil-half-cursor
  :enable (normal evil-operator-shortcut-map))

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
  (let (beg end interactive keys type keyword)
    ;; collect BEG, END and TYPE
    (setq args (delq '&optional args)
          beg (or (pop args) 'beg)
          end (or (pop args) 'end)
          type (or (pop args) 'type))
    ;; collect docstring
    (when (stringp (car body))
      (setq doc (pop body)))
    ;; collect keywords
    (while (keywordp (setq keyword (car body)))
      (setq body (cdr body))
      (cond
       ((eq keyword :keys)
        (setq keys (pop body))
        (when (or (stringp keys) (not (listp keys)))
          (setq keys (list keys))))
       (t
        (pop body))))
    ;; collect `interactive' specification
    (when (eq (car-safe (car-safe body)) 'interactive)
      (setq interactive (cdr (pop body))))
    ;; macro expansion
    `(progn
       (add-to-list 'evil-operators ',operator t)
       (dolist (key ',keys)
         (define-key evil-operator-state-map key ',operator))
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
    (evil-save-state
      (evil-save-echo-area
        (cond
         ((evil-visual-state-p)
          (list (region-beginning) (region-end)))
         (t
          (evil-operator-state)
          (setq motion (evil-keypress-parser)
                evil-this-motion-count (cadr motion)
                evil-this-motion (car motion)
                evil-this-type nil)
	  (cond
	   (evil-repeat-count
	    (setq evil-this-motion-count evil-repeat-count)
	    ;; only the count of the first operator is overwritten
	    (setq evil-repeat-count nil))
	   ((or current-prefix-arg evil-this-motion-count)
            (setq evil-this-motion-count
                  (* (prefix-numeric-value current-prefix-arg)
                     (prefix-numeric-value evil-this-motion-count)))))
          (cond
           ((or (null evil-this-motion)
                (eq evil-this-motion 'keyboard-quit))
            (setq quit-flag t))
           (t
            (setq range (evil-motion-range evil-this-motion
                                           evil-this-motion-count
                                           (or type
                                               (evil-type motion)
                                               'exclusive))
                  beg (nth 0 range)
                  end (nth 1 range)
                  evil-this-type (nth 2 range))
            (list beg end)))))))))

(defun evil-motion-range (motion &optional count type)
  "Execute a motion and return the buffer positions.
The return value is a list (BEG END TYPE)."
  (let (beg end range)
    (evil-save-transient-mark
      (save-excursion
        (transient-mark-mode 1)
        (setq evil-motion-marker (move-marker (make-marker) (point)))
        (unwind-protect
            (let ((current-prefix-arg count))
              (condition-case nil
                  (call-interactively motion)
                (error nil))
              (cond
               ;; if text has been selected (i.e., it's a text object),
               ;; return the selection
               ((or (evil-visual-state-p)
                    (region-active-p))
                (cond
                 (evil-expand (region-beginning) (region-end) type)))
               (t
                (evil-expand evil-motion-marker (point) type))))
          ;; delete marker so it doesn't slow down editing
          (move-marker evil-motion-marker nil)
          (setq evil-motion-marker nil))))))

(defun evil-keypress-parser ()
  "Read from keyboard and build a command description.
Returns (CMD COUNT), where COUNT is the numeric prefix argument.
Both COUNT and CMD may be nil."
  (let ((inhibit-quit t)
        char digit keys cmd count)
    (while (progn
             (setq char (read-event))
             (when (symbolp char)
               (setq char (or (get char 'ascii-character) char)))
             ;; this trick from simple.el's `digit-argument'
             ;; converts keystrokes like C-0 and C-M-1 to digits
             (if (or (characterp char) (integerp char))
                 (setq digit (- (logand char ?\177) ?0))
               (setq digit nil))
             (if (keymapp cmd)
                 (setq keys (vconcat keys (vector char)))
               (setq keys (vector char)))
             (setq cmd (key-binding keys t))
             (cond
              ;; if CMD is a keymap, we need to read more
              ((keymapp cmd)
               t)
              ;; numeric prefix argument
              ((or (memq cmd '(digit-argument))
                   (and (eq (length keys) 1)
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
                    (> (length keys) 1)
                    (memq digit '(0 1 2 3 4 5 6 7 8 9)))
               (setq count (concat (or count "")
                                   (number-to-string digit)))
               ;; remove the digit from the key sequence
               ;; so we can see if the previous one goes anywhere
               (setq keys (evil-truncate-vector keys -1))
               (setq cmd (key-binding keys))
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

(evil-define-operator evil-rot13 (beg end)
  "ROT13 encrypt text."
  :keys "g?"
  (rot13-region beg end))

(provide 'evil-operators)

;;; evil-operators.el ends here
