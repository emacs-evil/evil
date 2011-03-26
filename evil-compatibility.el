;;;; Compatibility with different Emacs versions

;; Emacs < 23 does not know characterp
(unless (fboundp 'characterp)
  (defalias 'characterp 'char-valid-p))

(unless (fboundp 'region-active-p)
  (defun region-active-p ()
    "Returns t iff region and mark are active."
    (and transient-mark-mode mark-active)))

;; In older versions of Emacs, `called-interactively-p' takes
;; no arguments. Emacs 23.2 and newer takes one argument.
(defmacro evil-called-interactively-p ()
  (if (version< emacs-version "23.2")
      '(called-interactively-p)
    '(called-interactively-p 'any)))

(provide 'evil-compatibility)

;;; evil-compatibility.el ends here
