;;;; Integrate Evil with other modules

(require 'evil-core)
(require 'evil-motions)
(require 'evil-repeat)

(mapc 'evil-declare-motion evil-motions)
(mapc 'evil-declare-not-repeat
      '(digit-argument
        negative-argument
        save-buffer
        universal-argument
        universal-argument-minus
        universal-argument-other-key))
(mapc 'evil-declare-change-repeat
      '(dabbrev-expand
        hippie-expand))
(mapc 'evil-declare-abort-repeat
      '(eval-expression
        execute-extended-command
        compile
        recompile))

(dolist (map evil-overriding-maps)
  (eval-after-load (cdr map)
    `(evil-make-overriding-map ,(car map))))

(dolist (map evil-intercept-maps)
  (eval-after-load (cdr map)
    `(evil-make-intercept-map ,(car map))))

;;; Buffer-menu

(evil-declare-key 'motion Buffer-menu-mode-map
  "h" 'evil-backward-char
  "j" 'evil-next-line
  "k" 'evil-previous-line
  "l" 'evil-forward-char)

;;; Dired

(eval-after-load 'dired
  '(progn
     ;; use the standard Dired bindings as a base
     (evil-make-overriding-map dired-mode-map 'normal t)
     (evil-define-key 'normal dired-mode-map
       "h" 'evil-backward-char
       "j" 'evil-next-line
       "k" 'evil-previous-line
       "l" 'evil-forward-char
       "J" 'dired-goto-file       ; "j"
       "K" 'dired-do-kill-lines   ; "k"
       "r" 'dired-do-redisplay))) ; "l"

(eval-after-load 'wdired
  '(progn
     (add-hook 'wdired-mode-hook 'evil-change-to-initial-state)
     (defadvice wdired-change-to-dired-mode (after evil activate)
       (evil-change-to-initial-state nil t))))

;;; ELP

(eval-after-load 'elp
  '(defadvice elp-results (after evil activate)
     (evil-motion-state)))

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

(evil-declare-key 'motion Info-mode-map
  (kbd "\M-h") 'Info-help   ; "h"
  "\C-t" 'Info-history-back ; "l"
  "\C-o" 'Info-history-back
  " " 'Info-scroll-up
  (kbd "RET") 'Info-follow-nearest-node
  "\C-]" 'Info-follow-nearest-node
  (kbd "DEL") 'Info-scroll-down)

;;; Parentheses

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

;;; Speedbar

(evil-declare-key 'motion speedbar-key-map
  "h" 'backward-char
  "j" 'speedbar-next
  "k" 'speedbar-prev
  "l" 'forward-char
  "i" 'speedbar-item-info
  "r" 'speedbar-refresh
  "u" 'speedbar-up-directory
  "o" 'speedbar-toggle-line-expansion
  (kbd "RET") 'speedbar-edit-line)

;;; Undo tree visualizer

(defadvice undo-tree-visualize (after evil activate)
  "Initialize Evil in the visualization buffer."
  (when evil-local-mode
    (evil-initialize-state)))

(when (boundp 'undo-tree-visualizer-map)
  (define-key undo-tree-visualizer-map [remap evil-backward-char]
    'undo-tree-visualize-switch-branch-left)
  (define-key undo-tree-visualizer-map [remap evil-forward-char]
    'undo-tree-visualize-switch-branch-right)
  (define-key undo-tree-visualizer-map [remap evil-next-line]
    'undo-tree-visualize-redo)
  (define-key undo-tree-visualizer-map [remap evil-previous-line]
    'undo-tree-visualize-undo))

;;; Auto-complete
(eval-after-load 'auto-complete
  '(progn
     (evil-set-command-properties 'ac-complete :repeat 'evil-ac-repeat)
     (evil-set-command-properties 'ac-expand :repeat 'evil-ac-repeat)
     (evil-set-command-properties 'ac-next :repeat 'ignore)
     (evil-set-command-properties 'ac-previous :repeat 'ignore)

     (defvar evil-ac-prefix-len nil
       "The length of the prefix of the current item to be completed.")

     (defun evil-ac-repeat (flag)
       "Record the changes for auto-completion."
       (cond
        ((eq flag 'pre)
         (setq evil-ac-prefix-len (length ac-prefix))
         (evil-repeat-start-record-changes))
        ((eq flag 'post)
         ;; Add change to remove the prefix
         (evil-repeat-record-change (- evil-ac-prefix-len)
                                    ""
                                    evil-ac-prefix-len)
         ;; Add change to insert the full completed text
         (evil-repeat-record-change
          (- evil-ac-prefix-len)
          (buffer-substring-no-properties (- evil-repeat-pos
                                             evil-ac-prefix-len)
                                          (point))
          0)
         ;; Finish repeation
         (evil-repeat-finish-record-changes))))))

;; Eval last sexp
(defadvice preceding-sexp (around evil activate)
  "In normal-state, last sexp ends at point."
  (if (evil-normal-state-p)
      (save-excursion
        (unless (or (eobp) (eolp)) (forward-char))
        ad-do-it)
    ad-do-it))

(defadvice pp-last-sexp (around evil activate)
  "In normal-state, last sexp ends at point."
  (if (evil-normal-state-p)
      (save-excursion
        (unless (or (eobp) (eolp)) (forward-char))
        ad-do-it)
    ad-do-it))

(provide 'evil-integration)

;;; evil-integration.el ends here
