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

;; `read-key' is introduced in Emacs 23.2
(defun evil-read-key (&optional prompt)
  "Read a key from the keyboard.
Translates it according to the input method."
  (let ((old-global-map (current-global-map))
        (new-global-map (make-sparse-keymap))
        (overriding-terminal-local-map (make-sparse-keymap))
        overriding-local-map)
    (unwind-protect
        (progn
          (define-key new-global-map [menu-bar]
            (lookup-key global-map [menu-bar]))
          (define-key new-global-map [tool-bar]
            (lookup-key global-map [tool-bar]))
          (add-to-list 'new-global-map
                       (make-char-table nil 'self-insert-command) t)
          (use-global-map new-global-map)
          (aref (read-key-sequence prompt nil t) 0))
      (use-global-map old-global-map))))

(provide 'evil-compatibility)

;;; evil-compatibility.el ends here
