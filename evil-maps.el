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

(define-key evil-operator-state-map "g?" 'evil-rot13)

(provide 'evil-maps)

;;; evil-maps.el ends here
