;;; evil-integration.el --- Integrate Evil with other modules

;; Author: Vegard Øye <vegard_oye at hotmail.com>
;; Maintainer: Vegard Øye <vegard_oye at hotmail.com>

;; Version: 1.0.9

;;
;; This file is NOT part of GNU Emacs.

;;; License:

;; This file is part of Evil.
;;
;; Evil is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Evil is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Evil.  If not, see <http://www.gnu.org/licenses/>.

(require 'evil-maps)
(require 'evil-core)

;;; Code:

;;; Evilize some commands

(mapc #'(lambda (cmd)
          (evil-set-command-property cmd :keep-visual t)
          (evil-declare-not-repeat cmd))
      '(digit-argument
        negative-argument
        universal-argument
        universal-argument-minus
        universal-argument-more
        universal-argument-other-key))
(mapc #'evil-declare-not-repeat
      '(what-cursor-position))
(mapc #'evil-declare-change-repeat
      '(dabbrev-expand
        hippie-expand))
(mapc #'evil-declare-abort-repeat
      '(balance-windows
        eval-expression
        execute-extended-command
        exit-minibuffer
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

(dolist (cmd '(keyboard-quit keyboard-escape-quit))
  (evil-set-command-property cmd :suppress-operator t))

;;; Mouse
(evil-declare-insert-at-point-repeat 'mouse-yank-primary)
(evil-declare-insert-at-point-repeat 'mouse-yank-secondary)

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

;; Calling `keyboard-quit' should cancel repeat
(defadvice keyboard-quit (before evil activate)
  (when (fboundp 'evil-repeat-abort)
    (evil-repeat-abort)))

;; etags-select
;; FIXME: probably etags-select should be recomended in docs
(eval-after-load 'etags-select
  '(progn
     (define-key evil-motion-state-map "g]" 'etags-select-find-tag-at-point)))

;;; Buffer-menu

(evil-add-hjkl-bindings Buffer-menu-mode-map 'motion)

;; dictionary.el

(evil-add-hjkl-bindings dictionary-mode-map 'motion
  "?" 'dictionary-help        ; "h"
  "C-o" 'dictionary-previous) ; "l"

;;; Dired

(eval-after-load 'dired
  '(progn
     ;; use the standard Dired bindings as a base
     (evil-make-overriding-map dired-mode-map 'normal t)
     (evil-add-hjkl-bindings dired-mode-map 'normal
       "J" 'dired-goto-file                   ; "j"
       "K" 'dired-do-kill-lines               ; "k"
       "r" 'dired-do-redisplay                ; "l"
       ";" (lookup-key dired-mode-map ":")))) ; ":d", ":v", ":s", ":e"

(eval-after-load 'wdired
  '(progn
     (add-hook 'wdired-mode-hook #'evil-change-to-initial-state)
     (defadvice wdired-change-to-dired-mode (after evil activate)
       (evil-change-to-initial-state nil t))))

;;; ELP

(eval-after-load 'elp
  '(defadvice elp-results (after evil activate)
     (evil-motion-state)))

;;; ERT

(evil-add-hjkl-bindings ert-results-mode-map 'motion)

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
  (if (if (memq 'not evil-highlight-closing-paren-at-point-states)
          (memq evil-state evil-highlight-closing-paren-at-point-states)
        (not (memq evil-state evil-highlight-closing-paren-at-point-states)))
      ad-do-it
    (let ((pos (point)) syntax narrow)
      (setq pos
            (catch 'end
              (dotimes (var (1+ (* 2 evil-show-paren-range)))
                (if (zerop (mod var 2))
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
        (dolist (ov '(show-paren--overlay
                      show-paren--overlay-1
                      show-paren-overlay
                      show-paren-overlay-1))
          (let ((ov (and (boundp ov) (symbol-value ov))))
            (when (overlayp ov) (delete-overlay ov))))))))

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

;; Ibuffer
(eval-after-load 'ibuffer
  '(progn
     (evil-make-overriding-map ibuffer-mode-map 'normal t)
     (evil-define-key 'normal ibuffer-mode-map
       "j" 'evil-next-line
       "k" 'evil-previous-line
       "RET" 'ibuffer-visit-buffer)))

;;; Undo tree
(when (and (require 'undo-tree nil t)
           (fboundp 'global-undo-tree-mode))
  (global-undo-tree-mode 1))

(eval-after-load 'undo-tree
  '(progn
     (defun evil-turn-on-undo-tree-mode ()
       "Enable `undo-tree-mode' if evil is enabled.
This function enables `undo-tree-mode' when Evil is activated in
some buffer, but only if `global-undo-tree-mode' is also
activated."
       (when global-undo-tree-mode (undo-tree-mode 1)))

     (add-hook 'evil-local-mode-hook #'evil-turn-on-undo-tree-mode)

     (defadvice undo-tree-visualize (after evil activate)
       "Initialize Evil in the visualization buffer."
       (when evil-local-mode
         (evil-initialize-state)))

     (when (fboundp 'undo-tree-visualize)
       (evil-ex-define-cmd "undol[ist]" 'undo-tree-visualize)
       (evil-ex-define-cmd "ul" 'undo-tree-visualize))

     (when (boundp 'undo-tree-visualizer-mode-map)
       (define-key undo-tree-visualizer-mode-map
         [remap evil-backward-char] 'undo-tree-visualize-switch-branch-left)
       (define-key undo-tree-visualizer-mode-map
         [remap evil-forward-char] 'undo-tree-visualize-switch-branch-right)
       (define-key undo-tree-visualizer-mode-map
         [remap evil-next-line] 'undo-tree-visualize-redo)
       (define-key undo-tree-visualizer-mode-map
         [remap evil-previous-line] 'undo-tree-visualize-undo)
       (define-key undo-tree-visualizer-mode-map
         [remap evil-ret] 'undo-tree-visualizer-set))

     (when (boundp 'undo-tree-visualizer-selection-mode-map)
       (define-key undo-tree-visualizer-selection-mode-map
         [remap evil-backward-char] 'undo-tree-visualizer-select-left)
       (define-key undo-tree-visualizer-selection-mode-map
         [remap evil-forward-char] 'undo-tree-visualizer-select-right)
       (define-key undo-tree-visualizer-selection-mode-map
         [remap evil-next-line] 'undo-tree-visualizer-select-next)
       (define-key undo-tree-visualizer-selection-mode-map
         [remap evil-previous-line] 'undo-tree-visualizer-select-previous)
       (define-key undo-tree-visualizer-selection-mode-map
         [remap evil-ret] 'undo-tree-visualizer-set))))

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

;; Show key
(defadvice quail-show-key (around evil activate)
  "Temporarily go to Emacs state"
  (evil-with-state emacs ad-do-it))

(defadvice describe-char (around evil activate)
  "Temporarily go to Emacs state"
  (evil-with-state emacs ad-do-it))

(provide 'evil-integration)

;;; evil-integration.el ends here
