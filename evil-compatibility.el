;;;; Compatibility with different Emacs versions

;; Emacs < 23 does not know characterp
(unless (fboundp 'characterp)
  (defalias 'characterp 'char-valid-p))

(unless (fboundp 'region-active-p)
  (defun region-active-p ()
    "Returns t iff region and mark are active."
    (and transient-mark-mode mark-active)))

;; In Emacs 22 `called-interactively-p' takes no arguments, newer
;; Emacs 23 versions take one argument.
(cond
 ((zerop (cdr (subr-arity (symbol-function 'called-interactively-p))))
  (defalias 'evil-called-interactively-p 'called-interactively-p))
 ((defsubst evil-called-interactively-p ()
    (called-interactively-p 'interactive))))

(provide 'evil-compatibility)

;;; evil-compatibility.el ends here
