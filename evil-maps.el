;;;; Default keymaps

(require 'evil-states)
(require 'evil-insert)
(require 'evil-operators)
(require 'evil-window)

(defmacro evil-redirect-digit-argument (map keys target)
  "Bind a special wrapper function which calles either `target' or `digit-argument'.
`digit-argument' is only called if a prefix-argument has already been started, otherwise `target' is called.
MAP    the keymap where the command should be bound
KEYS   the key-sequence to which the command should be bound
TARGET the command to call."
  (let ((wrapper (intern (concat "evil-digit-argument-or-"
                                 (symbol-name (eval target))))))
    `(progn
       (defun ,wrapper ()
         (interactive)
         (if current-prefix-arg
             (progn
               (setq this-command 'digit-argument)
               (call-interactively 'digit-argument))
           (setq this-command ,target)
           (call-interactively ,target)))
       (put ',wrapper 'evil-digit-argument-redirection t)
       (define-key ,map ,keys ',wrapper))))

(define-key evil-emacs-state-map "\C-z" 'evil-normal-state)

(define-key evil-normal-state-map "\C-z" 'evil-emacs-state)
(define-key evil-normal-state-map "a" 'evil-insert-after)
(define-key evil-normal-state-map "i" 'evil-insert-before)
(define-key evil-normal-state-map "O" 'evil-insert-above)
(define-key evil-normal-state-map "o" 'evil-insert-below)
(define-key evil-normal-state-map "I" 'evil-insert-beginning-of-line)
(define-key evil-normal-state-map "A" 'evil-insert-end-of-line)
(define-key evil-normal-state-map "x" 'delete-char)
(define-key evil-normal-state-map "r" 'evil-replace-char)
(define-key evil-normal-state-map "." 'evil-repeat)

(define-key evil-insert-state-map [escape] 'evil-normal-state)

;; "0" is a special command when called first
(evil-redirect-digit-argument evil-motion-state-map "0" 'evil-beginning-of-line)
(define-key evil-motion-state-map "1" 'digit-argument)
(define-key evil-motion-state-map "2" 'digit-argument)
(define-key evil-motion-state-map "3" 'digit-argument)
(define-key evil-motion-state-map "4" 'digit-argument)
(define-key evil-motion-state-map "5" 'digit-argument)
(define-key evil-motion-state-map "6" 'digit-argument)
(define-key evil-motion-state-map "7" 'digit-argument)
(define-key evil-motion-state-map "8" 'digit-argument)
(define-key evil-motion-state-map "9" 'digit-argument)

(define-key evil-motion-state-map "l" 'evil-forward-char)
(define-key evil-motion-state-map "h" 'evil-backward-char)
(define-key evil-motion-state-map "k" 'evil-previous-line)
(define-key evil-motion-state-map "j" 'evil-next-line)
(define-key evil-motion-state-map "H" 'evil-move-to-window-line)
(define-key evil-motion-state-map "M" 'evil-move-to-middle-window-line)
(define-key evil-motion-state-map "L" 'evil-move-to-last-window-line)
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

(define-key evil-motion-state-map "w" 'evil-forward-word-begin)
(define-key evil-motion-state-map "e" 'evil-forward-word-end)
(define-key evil-motion-state-map "b" 'evil-backward-word-begin)
(define-key evil-motion-state-map "ge" 'evil-backward-word-end)
(define-key evil-motion-state-map "W" 'evil-forward-WORD-begin)
(define-key evil-motion-state-map "E" 'evil-forward-WORD-end)
(define-key evil-motion-state-map "B" 'evil-backward-WORD-begin)
(define-key evil-motion-state-map "gE" 'evil-backward-WORD-end)

(define-key evil-motion-state-map "(" 'evil-backward-sentence)
(define-key evil-motion-state-map ")" 'evil-forward-sentence)
(define-key evil-motion-state-map "{" 'evil-backward-paragraph)
(define-key evil-motion-state-map "}" 'evil-forward-paragraph)

(define-key evil-normal-state-map "p" 'evil-paste-behind)
(define-key evil-normal-state-map "P" 'evil-paste-before)
(define-key evil-normal-state-map "\C-p" 'evil-paste-pop)
(define-key evil-normal-state-map "\C-n" 'evil-paste-pop-next)
(define-key evil-operator-state-map "y" 'evil-yank)
(define-key evil-operator-state-map "d" 'evil-delete)
(define-key evil-operator-state-map "c" 'evil-change)
(define-key evil-operator-state-map "g?" 'evil-rot13)

(define-key evil-motion-state-map (kbd "C-e") 'evil-scroll-line-down)
(define-key evil-motion-state-map (kbd "C-d") 'evil-scroll-down)
(define-key evil-motion-state-map (kbd "C-f") 'evil-scroll-page-down)
(define-key evil-motion-state-map "z+" 'evil-scroll-bottom-line-to-top)

(define-key evil-motion-state-map (kbd "C-y") 'evil-scroll-line-up)
(define-key evil-motion-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-motion-state-map (kbd "C-b") 'evil-scroll-page-up)
(define-key evil-motion-state-map "z^" 'evil-scroll-top-line-to-bottom)

(define-key evil-motion-state-map "zt" 'evil-scroll-line-to-top)
;; TODO: z RET has an advanced form taking an count before the RET
;; but this requires again a special state with a single command
;; bound to RET
(define-key evil-motion-state-map (vconcat "z" [return]) "zt^")
(define-key evil-motion-state-map (kbd "z RET") (vconcat "z" [return]))
(define-key evil-motion-state-map "zz" 'evil-scroll-line-to-center)
(define-key evil-motion-state-map "z." "zz^")
(define-key evil-motion-state-map "zb" 'evil-scroll-line-to-bottom)
(define-key evil-motion-state-map "z-" "zb^")

(define-key evil-normal-state-map (kbd "C-w +") 'evil-window-increase-height)
(define-key evil-normal-state-map (kbd "C-w -") 'evil-window-decrease-height)
(define-key evil-normal-state-map (kbd "C-w >") 'evil-window-increase-width)
(define-key evil-normal-state-map (kbd "C-w <") 'evil-window-decrease-width)
(define-key evil-normal-state-map (kbd "C-w H") 'evil-window-move-far-left)
(define-key evil-normal-state-map (kbd "C-w J") 'evil-window-move-very-bottom)
(define-key evil-normal-state-map (kbd "C-w K") 'evil-window-move-very-top)
(define-key evil-normal-state-map (kbd "C-w L") 'evil-window-move-far-right)
(define-key evil-normal-state-map (kbd "C-w =") 'balance-windows)
(define-key evil-normal-state-map (kbd "C-w R") 'evil-window-rotate-upwards)
(define-key evil-normal-state-map (kbd "C-w C-R") (kbd "C-w R"))
(define-key evil-normal-state-map (kbd "C-w r") 'evil-window-rotate-downwards)
(define-key evil-normal-state-map (kbd "C-w C-r") (kbd "C-w r"))
(define-key evil-normal-state-map (kbd "C-w _") 'evil-window-set-height)
(define-key evil-normal-state-map (kbd "C-w C-_") (kbd "C-w _"))
(define-key evil-normal-state-map (kbd "C-w |") 'evil-window-set-width)
(define-key evil-normal-state-map (kbd "C-w b") 'evil-window-bottom-right)
(define-key evil-normal-state-map (kbd "C-w C-b") (kbd "C-w b"))
(define-key evil-normal-state-map (kbd "C-w t") 'evil-window-top-left)
(define-key evil-normal-state-map (kbd "C-w C-t") (kbd "C-w t"))
(define-key evil-normal-state-map (kbd "C-w c") 'delete-window)
(define-key evil-normal-state-map (kbd "C-w h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-w C-h") (kbd "C-w h"))
(define-key evil-normal-state-map (kbd "C-w j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-w C-j") (kbd "C-w j"))
(define-key evil-normal-state-map (kbd "C-w k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-w C-k") (kbd "C-w k"))
(define-key evil-normal-state-map (kbd "C-w l") 'evil-window-right)
(define-key evil-normal-state-map (kbd "C-w C-l") (kbd "C-w l"))
(define-key evil-normal-state-map (kbd "C-w p") 'evil-window-lru)
(define-key evil-normal-state-map (kbd "C-w C-p") (kbd "C-w p"))
(define-key evil-normal-state-map (kbd "C-w w") 'evil-window-next)
(define-key evil-normal-state-map (kbd "C-w C-w") (kbd "C-w w"))
(define-key evil-normal-state-map (kbd "C-w W") 'evil-window-prev)
(define-key evil-normal-state-map (kbd "C-w n") 'evil-window-new)
(define-key evil-normal-state-map (kbd "C-w C-n") (kbd "C-w n"))
(define-key evil-normal-state-map (kbd "C-w o") 'delete-other-windows)
(define-key evil-normal-state-map (kbd "C-w C-o") (kbd "C-w o"))
(define-key evil-normal-state-map (kbd "C-w s") 'split-window-vertically)
(define-key evil-normal-state-map (kbd "C-w C-s") (kbd "C-w s"))
(define-key evil-normal-state-map (kbd "C-w S") (kbd "C-w s"))
(define-key evil-normal-state-map (kbd "C-w v") 'split-window-horizontally)
(define-key evil-normal-state-map (kbd "C-w C-v") (kbd "C-w v"))

(provide 'evil-maps)

;;; evil-maps.el ends here
