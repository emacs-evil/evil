;;;; Common functions and utilities

(require 'evil-vars)
(require 'evil-compatibility)

(defun evil-motion-p (cmd)
  "Return non-nil if CMD is a motion."
  (memq cmd evil-motions))

(defun evil-operator-p (cmd)
  "Return non-nil if CMD is an operator."
  (memq cmd evil-operators))

(defun evil-add-to-alist (list-var key val &rest elements)
  "Add the assocation of KEY and VAL to the value of LIST-VAR.
If the list already contains an entry for KEY, update that entry;
otherwise add at the end of the list."
  (let ((tail (symbol-value list-var)))
    (while (and tail (not (equal (car-safe (car-safe tail)) key)))
      (setq tail (cdr tail)))
    (if tail
        (setcar tail (cons key val))
      (add-to-list list-var (cons key val) t))
    (if elements
        (apply 'evil-add-to-alist list-var elements)
      (symbol-value list-var))))

(defun evil-concat-lists (&rest sequences)
  "Concatenate lists, removing duplicates.
The first occurrence is retained.

To concatenate association lists, see `evil-concat-alists'."
  (let ((first (pop sequences))
        (tail (copy-sequence (pop sequences)))
        result)
    ;; remove internal duplicates
    (dolist (elt first)
      (add-to-list 'result elt t 'eq))
    ;; remove tail duplicates
    (catch 'empty
      (dolist (elt result)
        (if tail
            (setq tail (delq elt tail))
          (throw 'empty t))))
    (setq result (append result tail))
    (if sequences
        (apply 'evil-concat-lists result sequences)
      result)))

(defun evil-concat-alists (&rest sequences)
  "Concatenate association lists, removing duplicates.
The first association is retained.

To concatenate regular lists, see `evil-concat-lists'."
  (let ((first (pop sequences))
        (tail (copy-sequence (pop sequences)))
        result)
    ;; remove internal duplicates
    (dolist (elt first)
      (unless (assq (car-safe elt) result)
        (add-to-list 'result elt t 'eq)))
    ;; remove tail duplicates
    (catch 'empty
      (dolist (elt result)
        (if tail
            (setq tail (assq-delete-all (car-safe elt) tail))
          (throw 'empty t))))
    (setq result (append result tail))
    (if sequences
        (apply 'evil-concat-lists result sequences)
      result)))

(defun evil-get-property (alist key prop)
  "Return property PROP for KEY in ALIST.
ALIST is an association list with entries in the form
\(KEY . PLIST), where PLIST is a property list.
If KEY is nil, return an association list of states and
their PROP values."
  (let (result val)
    (unless (keywordp prop)
      (setq prop (intern (format ":%s" prop))))
    (if key
        (plist-get (cdr (assq key alist)) prop)
      (dolist (entry alist result)
        (setq key (car entry)
              val (plist-get (cdr entry) prop))
        (when val
          (add-to-list 'result (cons key val) t))))))

(defun evil-put-property (alist-var key prop val &rest properties)
  "Set PROP to VAL for KEY in ALIST-VAR.
ALIST-VAR points to an association list with entries in the form
\(KEY . PLIST), where PLIST is a property list storing PROP and VAL."
  (let* ((alist (symbol-value alist-var))
         (plist (cdr (assq key alist))))
    (while
        (progn
          (unless (keywordp prop)
            (setq prop (intern (format ":%s" prop))))
          (setq plist (plist-put plist prop val))
          (when properties
            (setq prop (pop properties)
                  val (pop properties)))))
    (set alist-var (assq-delete-all key alist))
    (add-to-list alist-var (cons key plist) t)))

(defmacro evil-swap (this that &rest vars)
  "Swap the values of variables THIS and THAT.
If three or more arguments are given, the values are rotated.
E.g., (evil-swap A B C) sets A to B, B to C, and C to A."
  `(progn
     (setq ,this (prog1 ,that
                   (setq ,that ,this)))
     ,@(when vars
         `((evil-swap ,that ,@vars)))))

(defmacro evil-sort (min max &rest vars)
  "Place the smallest value in MIN and the largest in MAX.
If three or more arguments are given, place the smallest
value in the first argument and the largest in the last,
sorting in between."
  `(let ((sorted (sort (list ,min ,max ,@vars) '<)))
     (setq ,min (pop sorted)
           ,max (pop sorted)
           ,@(let (forms)
               (while vars
                 (add-to-list 'forms (pop vars) t)
                 (add-to-list 'forms '(pop sorted) t))
               forms))))

;; toggleable version of `with-temp-message'
(defmacro evil-save-echo-area (&rest body)
  "Save the echo area; execute BODY; restore the echo area.
Intermittent messages are not logged in the *Messages* buffer."
  (declare (indent defun)
           (debug t))
  `(let ((old-msg (current-message))
         evil-write-echo-area)
     (unwind-protect
         (progn ,@body)
       (unless evil-write-echo-area
         (if old-msg (message "%s" old-msg)
           (message nil))))))

(defun evil-echo (string &rest args)
  "Display an unlogged message in the echo area.
That is, the message is not logged in the *Messages* buffer.
\(To log the message, just use `message'.)"
  (let (message-log-max)
    (apply 'message string args)))

(defmacro evil-save-state (&rest body)
  "Save the current state; execute BODY; restore the state."
  (declare (indent defun)
           (debug t))
  `(let ((old-state evil-state))
     (unwind-protect
         (progn ,@body)
       (evil-change-state old-state))))

(defmacro evil-with-state (state &rest body)
  "Change to STATE; execute BODY; restore previous state."
  (declare (indent defun)
           (debug t))
  `(evil-save-state
     (evil-change-state ',state)
     ,@body))

(defun evil-set-cursor (specs)
  "Change the cursor's apperance according to SPECS.
SPECS may be a cursor type as per `cursor-type', a color
string as passed to `set-cursor-color', a zero-argument
function for changing the cursor, or a list of the above.
If SPECS is nil, make the cursor a black filled box."
  (set-cursor-color "black")
  (setq cursor-type 'box)
  (unless (and (listp specs) (not (consp specs)))
    (setq specs (list specs)))
  (dolist (spec specs)
    (cond
     ((functionp spec)
      (condition-case nil
          (funcall spec)
        (error nil)))
     ((stringp spec)
      (set-cursor-color spec))
     (t
      (setq cursor-type spec))))
  (redisplay))

(defmacro evil-save-cursor (&rest body)
  "Save the current cursor; execute BODY; restore the cursor."
  (declare (indent defun)
           (debug t))
  `(let ((cursor cursor-type)
         (color (cdr (assq 'cursor-color (frame-parameters)))))
     (unwind-protect
         (progn ,@body)
       (evil-set-cursor cursor)
       (evil-set-cursor color))))

(defun evil-move-to-column (column &optional dir force)
  "Move point to column COLUMN in the current line.
Places point at left of the tab character (at the right if DIR
is non-nil) and returns point."
  (interactive "p")
  (move-to-column column force)
  (unless force
    (when (or (not dir) (and (numberp dir) (< dir 1)))
      (when (> (current-column) column)
        (unless (bolp)
          (backward-char)))))
  (point))

;;; Region

(defun evil-transient-save ()
  "Save Transient Mark mode and make the new setup buffer-local.
The variables to save are listed in `evil-transient-vars'.
Their values are stored in `evil-transient-vals'."
  (dolist (var evil-transient-vars)
    (when (and (boundp var)
               (not (assq var evil-transient-vals)))
      (add-to-list 'evil-transient-vals
                   (list var (symbol-value var)
                         (and (assq var (buffer-local-variables)) t)))
      (make-variable-buffer-local var))))

(defun evil-transient-restore ()
  "Restore Transient Mark mode from `evil-transient-vals'."
  (let (entry local var val)
    (while (setq entry (pop evil-transient-vals))
      (setq var (pop entry)
            val (pop entry)
            local (pop entry))
      (unless local
        (kill-local-variable var))
      (unless (equal (symbol-value var) val)
        (if (fboundp var)
            (funcall var (if var 1 -1))
          (setq var val))))))

;; In theory, an active region implies Transient Mark mode, and
;; disabling Transient Mark mode implies deactivating the region.
;; In practice, Emacs never clears `mark-active' except in Transient
;; Mark mode, so we define our own toggle functions to make things
;; more predictable.
(defun evil-transient-mark (&optional arg)
  "Toggle Transient Mark mode.
Ensure that the region is properly deactivated.
Enable with positive ARG, disable with negative ARG."
  (unless (numberp arg)
    (setq arg (if transient-mark-mode -1 1)))
  (cond
   ((< arg 1)
    (evil-active-region -1)
    (when transient-mark-mode
      (transient-mark-mode -1)))
   (t
    (unless transient-mark-mode
      (transient-mark-mode 1)))))

(defun evil-active-region (&optional arg)
  "Toggle active region.
Ensure that Transient Mark mode is properly enabled.
Enable with positive ARG, disable with negative ARG."
  (unless (numberp arg)
    (setq arg (if (region-active-p) -1 1)))
  (cond
   ((and (< arg 1))
    (when (or transient-mark-mode mark-active)
      (setq mark-active nil
            deactivate-mark nil)
      (run-hooks 'deactivate-mark-hook)))
   (t
    (evil-transient-mark 1)
    (when deactivate-mark
      (setq deactivate-mark nil))
    (unless (mark t)
      (evil-move-mark (point)))
    (unless (region-active-p)
      (set-mark (mark t))))))

(defmacro evil-save-region (&rest body)
  "Save Transient Mark mode, mark activation, mark and point.
Execute BODY, then restore those things."
  (declare (indent defun)
           (debug t))
  `(let (evil-transient-vals)
     (unwind-protect
         (save-excursion
           (evil-transient-save)
           ,@body)
       (evil-transient-restore))))

(defun evil-set-region (beg end &optional dir)
  "Set Emacs region to BEG and END.
Preserves the order of point and mark, unless specified by DIR:
a positive number means mark goes before or is equal to point,
a negative number means point goes before mark."
  (let* ((point (point))
         (mark (or (mark t) point))
         (dir (or dir (if (< point mark) -1 1))))
    (evil-sort beg end)
    (when (< dir 0)
      (evil-swap beg end))
    (evil-move-mark beg)
    (goto-char end)))

;; `set-mark' does too much at once
(defun evil-move-mark (pos)
  "Set buffer's mark to POS."
  (set-marker (mark-marker) pos))

;;; Key sequences

(defun evil-extract-count (keys)
  "Splits the key-sequence `keys' in prefix-argument part and the rest.
Returns two number (PREFIX CMD SEQ REST) where PREFIX is prefix
count, CMD the command to be executed, SEQ the subsequence
calling CMD and REST all remaining events in the key-sequence.
PREFIX and REST may be nil of they do not exist. If a command is
bound to some keyboard-macro it is expaned recursively."
  (catch 'done
    (let* ((len (length keys))
           (beg 0)
           (end 1)
           (found-prefix nil))
      (while (and (<= end len))
        (let ((cmd (key-binding (substring keys beg end))))
          (cond
           ((memq cmd '(undefined nil))
            (error "No command bound to %s" (substring keys beg end)))

           ((arrayp cmd) ; a keyboard macro, replace the command with the macro
            (setq keys (vconcat (substring keys 0 beg)
                                cmd
                                (substring keys end))
                  end (1+ beg)
                  len (length keys)))
           ((functionp cmd)
            (if (or (memq cmd '(digit-argument negative-argument))
                    (and found-prefix
                         (get cmd 'evil-digit-argument-redirection)))
                ;; skip those commands
                (setq found-prefix t ; we found at least one prefix argument
                      beg end
                      end (1+ end))
              ;; a real command, finish
              (throw 'done (list (and (not (zerop beg))
                                      (string-to-number
                                       (concat (substring keys 0 beg))))
                                 cmd
                                 (substring keys beg end)
                                 (and (< end len) (substring keys end))))))

           (t ;; append a further event
            (setq end (1+ end))))))
      (error "Key sequence contains no complete binding"))))


;;; Undo ring

(defmacro evil-with-undo (&rest body)
  "Executes the body with enabled undo. If undo is disabled in
the current buffer, the undo information is stored in
`evil-temporary-undo' instead of `buffer-undo-list'."
  (declare (indent defun)
           (debug t))
  `(let ((orig-buffer-undo-list t))
     (let (buffer-undo-list)
       ,@body
       (setq evil-temporary-undo (cons nil buffer-undo-list)))
     (unless (eq buffer-undo-list t)
       ;; Undo is enabled, so update the global buffer undo list.
       (setq buffer-undo-list (append evil-temporary-undo buffer-undo-list)
             evil-temporary-undo nil))))


(defun evil-undo-pop ()
  "Undos the last buffer change and removes the last undo
information from `buffer-undo-list'. If undo is disabled in the
current buffer, use the information of `evil-temporary-undo'
instead."
  (let ((paste-undo (list nil)))
    (let ((undo-list (if (eq buffer-undo-list t)
                         evil-temporary-undo
                       buffer-undo-list)))
      (when (or (not undo-list) (car undo-list))
        (error "Can't undo previous paste"))
      (pop undo-list) ;; remove 'nil
      (while (and undo-list
                  (car undo-list))
        (push (pop undo-list) paste-undo))
      (let ((buffer-undo-list (nreverse paste-undo))
            (orig-message (symbol-function 'message)))
        (fset 'message #'(lambda (&rest rest)))
        (undo)
        (fset 'message orig-message))
      (if (eq buffer-undo-list t)
          (setq evil-temporary-undo nil)
        (setq buffer-undo-list undo-list)))))


;;; Command properties

(defun evil-add-command-properties (command &rest properties)
  "Adds the evil properties of a COMMAND.
REST should be a list of an even number of values, the first of a pair considered
as a key, the second as the value. The properties are stored in an alist at
the symbol COMMAND's property list entry 'evil-properties."
  (let ((cmd-properties (get command 'evil-properties)))
    (when (>= (length properties) 2)
      (apply #'evil-add-to-alist 'cmd-properties properties))
    (put command 'evil-properties cmd-properties)))

(defun evil-set-command-properties (command &rest properties)
  "Sets the evil properties of a COMMAND.
REST should be a list of an even number of values, the first of a pair considered
as a key, the second as the value. The properties are stored in an alist at
the symbol COMMAND's property list entry 'evil-properties."
  (put command 'evil-properties nil)
  (apply #'evil-add-command-properties command properties))

(defun evil-has-properties-p (command)
  "Returns non-nil if and only if evil-properties are defined for COMMAND.
If no evil-properties are defined for COMMAND several parts of
evil apply certain default rules, e.g., the repeat-system decides
whether the command is repeatable by monitoring buffer changes."
  (and (get command 'evil-properties) t))

(defun evil-get-command-property (command prop)
  "Returns the value of evil-property PROP of command COMMAND."
  (cdr-safe (assq prop (get command 'evil-properties))))

(defun evil-repeatable-p (command)
  "Return non-nil iff COMMAND is repeatable."
  (evil-get-command-property command 'repeatable))

(defun evil-keep-visual-p (command)
  "Return non-nil iff COMMAND should not exit visual state."
  (evil-get-command-property command 'keep-visual))


;;; Highlighting

(when (fboundp 'font-lock-add-keywords)
  (font-lock-add-keywords
   'emacs-lisp-mode
   '(("(\\(evil-define-[-[:word:]]+\\)\\>[ \f\t\n\r\v]*\\(\\sw+\\)?"
      (1 font-lock-keyword-face)
      (2 font-lock-function-name-face nil t))
     ("(\\(evil-\\(?:with\\|save\\)-[-[:word:]]+\\)\\>"
      1 font-lock-keyword-face))))

(provide 'evil-common)

;;; evil-common.el ends here
