;;;; Compatibility with different Emacs versions

;; Emacs < 23 does not know characterp
(unless (fboundp 'characterp)
  (defalias 'characterp 'char-valid-p))

(unless (fboundp 'region-active-p)
  (defun region-active-p ()
    "Returns t iff region and mark are active."
    mark-active))

(provide 'evil-compatibility)

;;; evil-compatibility.el ends here
