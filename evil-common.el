;;;; Common functions and utilities

(require 'evil-vars)

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
      (setq var (nth 0 entry)
            val (nth 1 entry)
            local (nth 2 entry))
      (unless local
        (kill-local-variable var))
      (unless (equal var val)
        (if (fboundp var)
            (funcall var (if var 1 -1))
          (setq var val))))))

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
  `(progn
       (if (eq buffer-undo-list t)
           ;; Undo disabled
           (setq evil-temporary-undo t
                 buffer-undo-list nil)
         (setq evil-temporary-undo nil))
       ,@body
       (when evil-temporary-undo
         ;; Undo disabled. Don't forget to add the undo-boundary at the
         ;; beginning, with undo enabled this would be done by the
         ;; Emacs main loop.
         (setq evil-temporary-undo (cons nil buffer-undo-list)
               buffer-undo-list t))))

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
