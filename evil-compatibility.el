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

(defun evil-read-key (&optional prompt)
  "Read a key from the keyboard.
Translates it according to the input method."
  (let ((old-global-map (current-global-map))
        (new-global-map (make-sparse-keymap))
        (overriding-terminal-local-map (make-sparse-keymap))
        overriding-local-map char)
    (unwind-protect
        (progn
          (define-key new-global-map [menu-bar]
            (lookup-key global-map [menu-bar]))
          (define-key new-global-map [tool-bar]
            (lookup-key global-map [tool-bar]))
          (add-to-list 'new-global-map
                       (make-char-table 'display-table
                                        'self-insert-command) t)
          (use-global-map new-global-map)
          (setq char (aref (read-key-sequence prompt nil t) 0))
          (if (memq char '(?\C-q ?\C-v))
              (read-quoted-char)
            (unless (eq char ?\e)
              (or (cdr (assq char '((?\r . ?\n))))
                  char))))
      (use-global-map old-global-map))))

;; `make-char-table' requires this property in Emacs 22
(unless (get 'display-table 'char-table-extra-slots)
  (put 'display-table 'char-table-extra-slots 0))

(provide 'evil-compatibility)

;;; evil-compatibility.el ends here
