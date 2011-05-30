;;;; Compatibility with different Emacs versions

;; Emacs <23 does not know `characterp'
(unless (fboundp 'characterp)
  (defalias 'characterp 'char-valid-p))

;; In older versions of Emacs, `called-interactively-p' takes
;; no arguments. In Emacs 23.2 and newer, it takes one argument.
(defmacro evil-called-interactively-p ()
  (if (version< emacs-version "23.2")
      '(called-interactively-p)
    '(called-interactively-p 'any)))

(unless (fboundp 'region-active-p)
  (defun region-active-p ()
    "Returns t iff region and mark are active."
    (and transient-mark-mode mark-active)))

(provide 'evil-compatibility)

;;; evil-compatibility.el ends here
