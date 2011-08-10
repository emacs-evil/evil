;;; Evil extended interactive forms.

(defvar evil-interactive-codes-alist nil
  "Alist of defined evil specific interactive codes.")

(defun evil-get-interactive-code (istring pos)
  "Lookup interactive code at position POS in ISTRING.
The return value is the first matching entry or nil."
  (let ((len (length istring)))
    (catch 'done
      (dolist (code evil-interactive-codes-alist)
        (let* ((cstr (car code))
               (clen (length cstr))
               (end (+ pos clen)))
          (when (and (<= end len)
                     (string= cstr (substring istring pos end)))
            (throw 'done code))))
      nil)))

(defmacro evil-define-interactive-code (code list-expr &rest attributes)
  "Defines a simple interactive code.
CODE is a string representing the interactive code. LIST-EXPR is
an expression that must evaluate to a list of arguments to be
passed to the command. ATTRIBUTES is list of keyword-value-pairs
to be passed to `evil-define-command' as implicit keyword
arguments."
  `(eval-and-compile
     (let ((c (assoc ,code evil-interactive-codes-alist))
           (v (cons ',list-expr ',attributes)))
       (if c (setcdr c v)
         (push (cons ,code v) evil-interactive-codes-alist)))))

(defmacro evil-define-interactive-code-function (code args &rest body)
  "Defines a complex interactive code.
CODE is a string representing the interactive code. ARGS is an
argument list with two arguments STRING and POS where STRING is
the interactive string to be parsed and POS the index of this
string where CODE starts. BODY is a list of expression (a
function body) which should return a list with three
elements (NEW-POS LIST-EXPR ATTRIBUTES) where NEW-POS is the
position of the first unparsed character in STRING, LIST-EXPR is
a list expression to be passed to the called command and
ATTRIBUTES is a list of keyword value pairs to be passed to
`evil-define-command'."
  (declare (indent defun)
           (debug (&define name
                           [&optional lambda-list]
                           [&optional stringp]
                           def-body)))
  (let ((name (intern (concat "evil-eval-interactive-" code)))
        doc)
    (when (stringp (car body))
      (setq doc (pop body)))
    `(progn
       (defun ,name ,args
         ,@(if doc (list doc))
         ,@body)
       (evil-define-interactive-code ,code t ',name))))

(defun evil-cleanup-interactive-form (form)
  "Simplifies the list of interactive expression FORM.
The returned value is a single list expression representing all
list expression in FORM where successive list expressions are
joined (if possible)."
  (if (null form) nil
    (let (new-form)
      (while (cdr form)
        (cond
         ((null (car form)) (pop form))
         ((and (eq (car (car form)) 'list)
               (eq (car (cadr form)) 'list))
          (setq form (cons (append (car form)
                                   (cdr (cadr form)))
                           (cdr (cdr form)))))
         (t
          (push (pop form) new-form))))
      (if (car form) (push (pop form) new-form))
      (setq new-form (nreverse new-form))
      (cond
       ((null new-form) nil)
       ((null (cdr new-form)) (car new-form))
       (t `(append ,@new-form))))))

(defun evil-eval-interactive-string (istring &optional pos)
  "Transforms the interactive code string ISTRING to a list expression starting at string position POS."
  (let ((pos (or pos 0))
        (len (length istring))
        result-form
        result-attr)
    (while (< pos len)
      (let ((code (evil-get-interactive-code istring pos)))
        (if (not code)
            (error "Unknown interactive code: '%c'" (aref istring pos))
          (if (eq (cadr code) t)
              ;; call function
              (let ((result (funcall (cdr (cdr code)) istring pos)))
                (setq pos (pop result))
                (setq result-form (append result-form (list (car result)))
                      result-attr (append result-attr (cdr result))))

            ;; use expression list
            (when (car (cdr code))
              (setq result-form (append result-form (list (car (cdr code))))))
            (when (cdr (cdr code))
              (setq result-attr (append result-attr (cdr (cdr code)))))
            (setq pos (+ pos (length (car code))))))))
    (cons (evil-cleanup-interactive-form result-form) result-attr)))

(defun evil-eval-interactive (&rest interactive-form)
  "Transforms the extended evil interactive form INTERACTIVE-FORM.
The return value is a pair (IFORM . ATTRIBUTES) where IFORM is a
single list-expression to be passed to a standard `interactive'
statement and ATTRIBUTES is a list of keyword-value pairs to be
passed to `evil-define-command'."
  (let (ilist iattr)
    (dolist (istmt interactive-form)
      (if (stringp istmt)
          (let ((result (evil-eval-interactive-string istmt)))
            (setq ilist (append ilist (list (car result)))
                  iattr (append iattr (cdr result))))
        (setq ilist (append ilist (list istmt)))))
    (cons (evil-cleanup-interactive-form ilist) iattr)))

(provide 'evil-interactive)
