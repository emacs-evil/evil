;;;; Integrate Evil with other modules

(require 'evil-states)
(require 'evil-motions)

(mapc 'evil-declare-motion evil-motions)
(mapc 'evil-declare-not-repeat '(save-buffer))
(mapc 'evil-declare-change-repeat '(dabbrev-expand hippie-expand))

;;; Buffer-menu

(eval-after-load "buff-menu"
  '(evil-define-key 'motion Buffer-menu-mode-map (kbd "RET")
     'Buffer-menu-this-window))

;;; Dired

(eval-after-load 'dired
  '(progn
     (evil-define-key 'normal dired-mode-map "h" 'evil-backward-char)
     (evil-define-key 'normal dired-mode-map "j" 'evil-next-line)
     (evil-define-key 'normal dired-mode-map "k" 'evil-previous-line)
     (evil-define-key 'normal dired-mode-map "l" 'evil-forward-char)
     (evil-define-key 'normal dired-mode-map "r" 'dired-do-redisplay) ; "l"
     ;; use the standard Dired bindings as a base
     (set-keymap-parent
      (evil-get-auxiliary-keymap dired-mode-map 'normal t)
      (assq-delete-all 'menu-bar (copy-keymap dired-mode-map)))))

(eval-after-load 'wdired
  '(progn
     (add-hook 'wdired-mode-hook 'evil-change-to-initial-state)
     (defadvice wdired-change-to-dired-mode (after evil activate)
       (evil-change-to-initial-state nil t))))

;;; Folding

(eval-after-load 'hideshow
  '(progn
     (defun evil-za ()
       (interactive)
       (hs-toggle-hiding)
       (hs-hide-level evil-fold-level))
     (defun evil-hs-setup ()
       (define-key evil-normal-state-map "za" 'evil-za)
       (define-key evil-normal-state-map "zm" 'hs-hide-all)
       (define-key evil-normal-state-map "zr" 'hs-show-all)
       (define-key evil-normal-state-map "zo" 'hs-show-block)
       (define-key evil-normal-state-map "zc" 'hs-hide-block))
     (add-hook 'hs-minor-mode-hook 'evil-hs-setup)))

;; load goto-chg.el if available
(condition-case nil
    (require 'goto-chg)
  (error nil))

;;; Info

(eval-after-load 'info
  '(progn
     (evil-define-key 'motion Info-mode-map "\C-t"
       'Info-history-back) ; "l"
     (evil-define-key 'motion Info-mode-map "\C-o"
       'Info-history-back)
     (evil-define-key 'motion Info-mode-map (kbd "\M-h")
       'Info-help) ; "h"
     (evil-define-key 'motion Info-mode-map " "
       'Info-scroll-up)
     (evil-define-key 'motion Info-mode-map (kbd "RET")
       'Info-follow-nearest-node)
     (evil-define-key 'motion Info-mode-map "\C-]"
       'Info-follow-nearest-node)
     (evil-define-key 'motion Info-mode-map [backspace]
       'Info-scroll-down)))

;;; Shell

(eval-after-load 'comint
  '(define-key comint-mode-map [remap evil-ret] 'comint-send-input))

;;; Undo tree visualizer

(evil-set-initial-state 'undo-tree-visualizer-mode 'motion)

(when (boundp 'undo-tree-visualizer-map)
  (define-key undo-tree-visualizer-map [remap evil-backward-char]
    'undo-tree-visualize-switch-branch-left)
  (define-key undo-tree-visualizer-map [remap evil-forward-char]
    'undo-tree-visualize-switch-branch-right)
  (define-key undo-tree-visualizer-map [remap evil-next-line]
    'undo-tree-visualize-redo)
  (define-key undo-tree-visualizer-map [remap evil-previous-line]
    'undo-tree-visualize-undo))

(defadvice show-paren-function (around evil)
  "Match parentheses in Normal state."
  (if (or (evil-insert-state-p)
          (evil-replace-state-p)
          (evil-emacs-state-p))
      ad-do-it
    (let ((pos (point)) syntax)
      (setq pos
            (catch 'end
              (dotimes (var (1+ (* 2 evil-show-paren-range)))
                (if (evenp var)
                    (setq pos (+ pos var))
                  (setq pos (- pos var)))
                (setq syntax (syntax-class (syntax-after pos)))
                (cond
                 ((eq syntax 4)
                  (throw 'end pos))
                 ((eq syntax 5)
                  (throw 'end (1+ pos)))))))
      (if pos
          (save-excursion
            (goto-char pos)
            ad-do-it)
        ;; prevent the preceding pair from being highlighted
        (when (overlayp show-paren-overlay)
          (delete-overlay show-paren-overlay))
        (when (overlayp show-paren-overlay-1)
          (delete-overlay show-paren-overlay-1))))))

(provide 'evil-integration)

;;; evil-integration.el ends here
