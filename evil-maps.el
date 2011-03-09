;;;; Default keymaps

(require 'evil-states)
(require 'evil-insert)
(require 'evil-operators)

(define-key evil-emacs-state-map "\C-z" 'evil-normal-state)

(define-key evil-normal-state-map "\C-z" 'evil-emacs-state)
(define-key evil-normal-state-map "a" 'evil-insert-after)
(define-key evil-normal-state-map "i" 'evil-insert-before)
(define-key evil-normal-state-map "O" 'evil-insert-above)
(define-key evil-normal-state-map "o" 'evil-insert-below)
(define-key evil-normal-state-map "x" 'delete-char)
(define-key evil-normal-state-map "r" 'evil-replace-char)
(define-key evil-normal-state-map "." 'evil-repeat)

(define-key evil-insert-state-map [escape] 'evil-normal-state)

(define-key evil-motion-state-map "l" 'evil-forward-char)
(define-key evil-motion-state-map "h" 'evil-backward-char)
(define-key evil-motion-state-map "k" 'evil-previous-line)
(define-key evil-motion-state-map "j" 'evil-next-line)
(define-key evil-motion-state-map "H" 'evil-move-to-window-line)
(define-key evil-motion-state-map "M" 'evil-move-to-middle-window-line)
(define-key evil-motion-state-map "L" 'evil-move-to-last-window-line)
(define-key evil-motion-state-map "0" 'evil-beginning-of-line-or-digit-argument)
(define-key evil-motion-state-map "$" 'evil-end-of-line)
(define-key evil-motion-state-map "^" 'evil-first-non-blank)
(define-key evil-motion-state-map "g_" 'evil-last-non-blank)
(define-key evil-motion-state-map "gg" 'evil-move-to-first-non-blank-beg)
(define-key evil-motion-state-map "G" 'evil-move-to-first-non-blank-end)

(define-key evil-motion-state-map "gk" 'evil-previous-visual-line)
(define-key evil-motion-state-map "gj" 'evil-next-visual-line)
(define-key evil-motion-state-map "g0" 'evil-beginning-of-visual-line)
(define-key evil-motion-state-map "g^" 'evil-first-non-blank-of-visual-line)
(define-key evil-motion-state-map "g$" 'evil-end-of-visual-line)

(define-key evil-operator-state-map "g?" 'evil-rot13)

(provide 'evil-maps)

;;; evil-maps.el ends here
