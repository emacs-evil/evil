;;;; Motions

(require 'evil-vars)
(require 'evil-common)
(require 'evil-states)
(require 'evil-types)
(require 'evil-compatibility)

(evil-define-state motion
  "Motion state"
  :tag " <M> ")

(defmacro evil-define-motion (motion args &rest body)
  "Define an motion command MOTION.
ARGS is the argument list, which must contain
at least one argument: the count."
  (declare (indent defun)
           (debug (&define name lambda-list
                           [&optional stringp]
                           [&rest keywordp sexp]
                           def-body)))
  (let (count interactive keyword type)
    ;; collect COUNT
    (setq args (delq '&optional args)
          count (or (pop args) 'count))
    (when args
      (add-to-list 'args '&optional))
    ;; collect docstring
    (when (stringp (car body))
      (setq doc (pop body)))
    ;; collect keywords
    (while (keywordp (setq keyword (car body)))
      (setq body (cdr body))
      (cond
       ((eq keyword :type)
        (setq type (pop body)))
       (t
        (pop body))))
    ;; collect `interactive' specification
    (when (eq (car-safe (car-safe body)) 'interactive)
      (setq interactive (cdr (pop body))))
    ;; macro expansion
    `(progn
       (add-to-list 'evil-motions ',motion t)
       (when ',type
         (evil-set-type ',motion ',type))
       (defun ,motion (,count ,@args)
         ,@(when doc `(,doc))
         (interactive
          (append (list (prefix-numeric-value
                         current-prefix-arg))
                  ,@interactive))
         ,@body))))

(evil-define-motion evil-forward-char (count)
  "Move cursor to the right by COUNT characters."
  :type inclusive
  (save-restriction
    (narrow-to-region
     (line-beginning-position)
     (if (evil-visual-state-p)
         (line-end-position)
       (max (line-beginning-position)
            (1- (line-end-position)))))
    (condition-case nil
        (forward-char count)
      (error (error "End of line")))))

(provide 'evil-motions)

;;; evil-motions.el ends here
