;;;; Integrate Evil with other modules

(require 'evil-maps)

(mapc #'evil-declare-motion evil-motions)
(mapc #'evil-declare-not-repeat
      '(digit-argument
        negative-argument
        universal-argument
        universal-argument-minus
        universal-argument-other-key
        what-cursor-position))
(mapc #'evil-declare-change-repeat
      '(dabbrev-expand
        hippie-expand))
(mapc #'evil-declare-abort-repeat
      '(balance-windows
        eval-expression
        execute-extended-command
        compile
        delete-window
        delete-other-windows
        find-file-at-point
        ffap-other-window
        recompile
        save-buffer
        split-window
        split-window-horizontally
        split-window-vertically))

(evil-set-type #'previous-line 'line)
(evil-set-type #'next-line 'line)

(dolist (cmd evil-visual-newline-commands)
  (evil-add-command-properties cmd :exclude-newline t))

(dolist (map evil-overriding-maps)
  (eval-after-load (cdr map)
    `(evil-make-overriding-map ,(car map))))

(dolist (map evil-intercept-maps)
  (eval-after-load (cdr map)
    `(evil-make-intercept-map ,(car map))))

;;; key-binding

;; disable evil-esc-mode during a call to key-binding
(defadvice key-binding (around evil activate)
  (let (evil-esc-mode)
    ad-do-it))

;; disable evil-esc-mode during the read of a key-sequence
;; TODO: should we handle the special ESC-delay, too?
(defadvice read-key-sequence (around evil activate)
  (let (evil-esc-mode)
    ad-do-it))

;; disable evil-esc-mode during the read of a key-sequence
;; TODO: should we handle the special ESC-delay, too?
(defadvice read-key-sequence-vector (around evil activate)
  (let (evil-esc-mode)
    ad-do-it))

;;; Buffer-menu

(evil-add-hjkl-bindings Buffer-menu-mode-map 'motion)

;; dictionary.el

(evil-add-hjkl-bindings Buffer-menu-mode-map 'motion
  "?" 'dictionary-help) ; "h"

;;; Dired

(eval-after-load 'dired
  '(progn
     ;; use the standard Dired bindings as a base
     (evil-make-overriding-map dired-mode-map 'normal t)
     (evil-add-hjkl-bindings dired-mode-map 'normal
       "J" 'dired-goto-file       ; "j"
       "K" 'dired-do-kill-lines   ; "k"
       "r" 'dired-do-redisplay))) ; "l"

(eval-after-load 'wdired
  '(progn
     (add-hook 'wdired-mode-hook #'evil-change-to-initial-state)
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
     (add-hook 'hs-minor-mode-hook #'evil-hs-setup)))

;; load goto-chg.el if available
(condition-case nil
    (require 'goto-chg)
  (error nil))

;;; Info

(evil-add-hjkl-bindings Info-mode-map 'motion
  "0" 'evil-digit-argument-or-evil-beginning-of-line
  (kbd "\M-h") 'Info-help   ; "h"
  "\C-t" 'Info-history-back ; "l"
  "\C-o" 'Info-history-back
  " " 'Info-scroll-up
  "\C-]" 'Info-follow-nearest-node
  (kbd "DEL") 'Info-scroll-down)

;;; Parentheses

(defadvice show-paren-function (around evil)
  "Match parentheses in Normal state."
  (if (or (evil-insert-state-p)
          (evil-replace-state-p)
          (evil-emacs-state-p))
      ad-do-it
    (let ((pos (point)) syntax narrow)
      (setq pos
            (catch 'end
              (dotimes (var (1+ (* 2 evil-show-paren-range)))
                (if (evenp var)
                    (setq pos (+ pos var))
                  (setq pos (- pos var)))
                (setq syntax (syntax-class (syntax-after pos)))
                (cond
                 ((eq syntax 4)
                  (setq narrow pos)
                  (throw 'end pos))
                 ((eq syntax 5)
                  (throw 'end (1+ pos)))))))
      (if pos
          (save-excursion
            (goto-char pos)
            (save-restriction
              (when narrow
                (narrow-to-region narrow (point-max)))
              ad-do-it))
        ;; prevent the preceding pair from being highlighted
        (when (overlayp show-paren-overlay)
          (delete-overlay show-paren-overlay))
        (when (overlayp show-paren-overlay-1)
          (delete-overlay show-paren-overlay-1))))))

;;; Speedbar

(evil-add-hjkl-bindings speedbar-key-map 'motion
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
     (evil-add-command-properties 'ac-complete :repeat 'evil-ac-repeat)
     (evil-add-command-properties 'ac-expand :repeat 'evil-ac-repeat)
     (evil-add-command-properties 'ac-next :repeat 'ignore)
     (evil-add-command-properties 'ac-previous :repeat 'ignore)

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
