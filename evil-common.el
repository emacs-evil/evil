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
The firstmost occurrence of an element is retained.
Cons cells are treated as alist entries."
  (let ((result (pop sequences))
        (tail (copy-sequence (pop sequences))))
    (catch 'empty
      (dolist (elt result)
        (cond
         ((null tail)
          (throw 'empty t))
         ((consp elt)
          (setq tail (assq-delete-all (car elt) tail)))
         (t
          (setq tail (delq elt tail))))))
    (if sequences
        (apply 'evil-concatenate-lists result sequences)
      (append result tail))))

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

(defun evil-truncate-vector (vector length &optional offset)
  "Return a copy of VECTOR truncated to LENGTH.
If LENGTH is negative, skip last elements of VECTOR.
If OFFSET is specified, skip first elements of VECTOR."
  ;; if LENGTH is too large, trim it
  (when (> length (length vector))
    (setq length (length vector)))
  ;; if LENGTH is negative, convert it to the positive equivalent
  (when (< length 0)
    (setq length (max 0 (+ (length vector) length))))
  (if offset
      (setq length (- length offset))
    (setq offset 0))
  (let ((result (make-vector length t)))
    (dotimes (idx length result)
      (aset result idx (aref vector (+ idx offset))))))

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

(defun evil-unlogged-message (string &rest args)
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
