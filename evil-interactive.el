;;; Extended interactive forms

(require 'evil-vars)

(defmacro evil-define-interactive-code (code &rest body)
  "Define an interactive code.
PROMPT, if given, is the remainder of the interactive string
up to the next newline. Command properties may be specified
via KEY-VALUE pairs. BODY should evaluate to a list of values.

\(fn CODE (PROMPT) [[KEY VALUE]...] BODY...)"
  (declare (indent defun))
  (let* ((args (when (and (> (length body) 1)
                          (listp (car-safe body)))
                 (pop body)))
         (doc (when (stringp (car-safe body)) (pop body)))
         func properties)
    (while (keywordp (car-safe body))
      (setq properties
            (append properties (list (pop body) (pop body)))))
    (cond
     (args
      (setq func `(lambda ,args
                    ,@(when doc `(,doc))
                    ,@body)))
     ((> (length body) 1)
      (setq func `(progn ,@body)))
     (t
      (setq func (car body))))
    `(eval-and-compile
       (let* ((code ,code)
              (entry (assoc code evil-interactive-alist))
              (value (cons ',func ',properties)))
         (if entry
             (setcdr entry value)
           (push (cons code value) evil-interactive-alist))
         code))))

(defun evil-match-interactive-code (interactive &optional pos)
  "Match an interactive code at position POS in string INTERACTIVE.
Returns the first matching entry in `evil-interactive-alist', or nil."
  (let ((length (length interactive))
        (pos (or pos 0)))
    (catch 'done
      (dolist (entry evil-interactive-alist)
        (let* ((string (car entry))
               (end (+ (length string) pos)))
          (when (and (<= end length)
                     (string= string
                              (substring interactive pos end)))
            (throw 'done entry)))))))

(defun evil-concatenate-interactive-forms (&rest forms)
  "Concatenate interactive list expressions FORMS.
Returns a single expression where successive expressions
are joined, if possible."
  (let (result)
    (when forms
      (while (cdr forms)
        (cond
         ((null (car forms))
          (pop forms))
         ((and (eq (car (car forms)) 'list)
               (eq (car (cadr forms)) 'list))
          (setq forms (cons (append (car forms)
                                    (cdr (cadr forms)))
                            (cdr (cdr forms)))))
         (t
          (push (pop forms) result))))
      (when (car forms)
        (push (pop forms) result))
      (setq result (nreverse result))
      (cond
       ((null result))
       ((null (cdr result))
        (car result))
       (t
        `(append ,@result))))))

(defun evil-interactive-string (string)
  "Evaluate the interactive string STRING.
The string may contain extended interactive syntax.
The return value is a cons cell (FORM . PROPERTIES),
where FORM is a single list-expression to be passed to
a standard `interactive' statement, and PROPERTIES is a
list of command properties as passed to `evil-define-command'."
  (let ((length (length string))
        (pos 0)
        code expr forms match plist prompt properties)
    (while (< pos length)
      (if (eq (aref string pos) ?\n)
          (setq pos (1+ pos))
        (setq match (evil-match-interactive-code string pos))
        (if (null match)
            (error "Unknown interactive code: `%c'"
                   (substring string pos))
          (setq code (car match)
                expr (car (cdr match))
                plist (cdr (cdr match))
                pos (+ pos (length code)))
          (when (functionp expr)
            (setq prompt
                  (substring string pos
                             (or (string-match "\n" string pos)
                                 length))
                  pos (+ pos (length prompt))
                  expr `(funcall ,expr ,prompt)))
          (setq forms (append forms (list expr))
                properties (append properties plist)))))
    (cons `(append ,@forms) properties)))

(defun evil-interactive-form (&rest args)
  "Evaluate interactive forms ARGS.
The return value is a cons cell (FORM . PROPERTIES),
where FORM is a single list-expression to be passed to
a standard `interactive' statement, and PROPERTIES is a
list of command properties as passed to `evil-define-command'."
  (let (forms properties)
    (dolist (arg args)
      (if (not (stringp arg))
          (setq forms (append forms (list arg)))
        (setq arg (evil-interactive-string arg))
        (setq forms (append forms (cdr (car arg)))
              properties (append properties (cdr arg)))))
    (cons (apply 'evil-concatenate-interactive-forms forms)
          properties)))

;;; Standard interactive codes

(evil-define-interactive-code "*"
  "Signal error if the buffer is read-only."
  (when buffer-read-only
    (signal 'buffer-read-only nil)))

(evil-define-interactive-code "b" (prompt)
  "Name of existing buffer."
  (list (read-buffer prompt (current-buffer) t)))

(evil-define-interactive-code "c"
  "Read character."
  (list (read-char)))

(evil-define-interactive-code "p"
  "Prefix argument converted to number."
  (list (prefix-numeric-value current-prefix-arg)))

(evil-define-interactive-code "P"
  "Prefix argument in raw form."
  (list current-prefix-arg))

;;; Custom interactive codes

(evil-define-interactive-code "<c>"
  "Count."
  (list (when current-prefix-arg
          (prefix-numeric-value
           current-prefix-arg))))

(evil-define-interactive-code "<r>"
  "Untyped motion range (BEG END)."
  (evil-operator-range))

(evil-define-interactive-code "<R>"
  "Typed motion range (BEG END TYPE)."
  (evil-operator-range t))

(evil-define-interactive-code "<x>"
  "Current register."
  (list evil-this-register))

(evil-define-interactive-code "<y>"
  "Current yank-handler."
  (list (evil-yank-handler)))

(evil-define-interactive-code "<f>"
  :ex-arg file
  (list (and (evil-ex-state-p) (evil-ex-file-arg))))

(evil-define-interactive-code "<b>"
  :ex-arg buffer
  (list (and (evil-ex-state-p) evil-ex-current-arg)))

(evil-define-interactive-code "<a>"
  :ex-arg t
  (list (and (evil-ex-state-p) evil-ex-current-arg)))

(evil-define-interactive-code "<!>"
  :ex-force t
  (list (and (evil-ex-state-p) evil-ex-current-cmd-force)))

(evil-define-interactive-code "<sym>"
  :ex-arg sym
  (list (and (evil-ex-state-p)
             evil-ex-current-arg
             (intern evil-ex-current-arg))))

(evil-define-interactive-code "<s/>"
  :ex-arg substitution
  (list (and (evil-ex-state-p) evil-ex-current-arg)))

(provide 'evil-interactive)
