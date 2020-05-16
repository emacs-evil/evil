;;; evil-maps.el --- Default keymaps -*- lexical-binding: t -*-

;; Author: Vegard Øye <vegard_oye at hotmail.com>
;; Maintainer: Vegard Øye <vegard_oye at hotmail.com>

;; Version: 1.14.0

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

(require 'evil-states)
(require 'evil-ex)
(require 'evil-commands)
(require 'evil-command-window)
(require 'evil-common)

;;; Code:

;;; Normal state

(define-key evil-normal-state-map "a" 'evil-append)
(define-key evil-normal-state-map "A" 'evil-append-line)
(define-key evil-normal-state-map "c" 'evil-change)
(define-key evil-normal-state-map "C" 'evil-change-line)
(define-key evil-normal-state-map "d" 'evil-delete)
(define-key evil-normal-state-map "D" 'evil-delete-line)
(define-key evil-normal-state-map "i" 'evil-insert)
(define-key evil-normal-state-map (kbd "<insert>") 'evil-insert)
(define-key evil-normal-state-map (kbd "<insertchar>") 'evil-insert)
(define-key evil-normal-state-map "I" 'evil-insert-line)
(define-key evil-normal-state-map "J" 'evil-join)
(define-key evil-normal-state-map "m" 'evil-set-marker)
(define-key evil-normal-state-map "o" 'evil-open-below)
(define-key evil-normal-state-map "O" 'evil-open-above)
(define-key evil-normal-state-map "p" 'evil-paste-after)
(define-key evil-normal-state-map "P" 'evil-paste-before)
(define-key evil-normal-state-map "q" 'evil-record-macro)
(define-key evil-normal-state-map "r" 'evil-replace)
(define-key evil-normal-state-map "R" 'evil-replace-state)
(define-key evil-normal-state-map "s" 'evil-substitute)
(define-key evil-normal-state-map "S" 'evil-change-whole-line)
(define-key evil-normal-state-map "x" 'evil-delete-char)
(define-key evil-normal-state-map "X" 'evil-delete-backward-char)
(define-key evil-normal-state-map [deletechar] 'evil-delete-char)
(define-key evil-normal-state-map "y" 'evil-yank)
(define-key evil-normal-state-map "Y" 'evil-yank-line)
(define-key evil-normal-state-map "&" 'evil-ex-repeat-substitute)
(define-key evil-normal-state-map "g&" 'evil-ex-repeat-global-substitute)
(define-key evil-normal-state-map "g8" 'what-cursor-position)
(define-key evil-normal-state-map "ga" 'what-cursor-position)
(define-key evil-normal-state-map "gi" 'evil-insert-resume)
(define-key evil-normal-state-map "gJ" 'evil-join-whitespace)
(define-key evil-normal-state-map "gq" 'evil-fill-and-move)
(define-key evil-normal-state-map "gw" 'evil-fill)
(define-key evil-normal-state-map "gu" 'evil-downcase)
(define-key evil-normal-state-map "gU" 'evil-upcase)
(define-key evil-normal-state-map "gf" 'find-file-at-point)
(define-key evil-normal-state-map "gF" 'evil-find-file-at-point-with-line)
(define-key evil-normal-state-map "gx" 'browse-url-at-point)
(define-key evil-normal-state-map "g?" 'evil-rot13)
(define-key evil-normal-state-map "g~" 'evil-invert-case)
(define-key evil-normal-state-map "zo" 'evil-open-fold)
(define-key evil-normal-state-map "zO" 'evil-open-fold-rec)
(define-key evil-normal-state-map "zc" 'evil-close-fold)
(define-key evil-normal-state-map "za" 'evil-toggle-fold)
(define-key evil-normal-state-map "zr" 'evil-open-folds)
(define-key evil-normal-state-map "zm" 'evil-close-folds)
(define-key evil-normal-state-map "z=" 'ispell-word)
(define-key evil-normal-state-map "\C-n" 'evil-paste-pop-next)
(define-key evil-normal-state-map "\C-p" 'evil-paste-pop)
(define-key evil-normal-state-map "\C-t" 'pop-tag-mark)
(define-key evil-normal-state-map (kbd "C-.") 'evil-repeat-pop)
(define-key evil-normal-state-map (kbd "M-.") 'evil-repeat-pop-next)
(define-key evil-normal-state-map "." 'evil-repeat)
(define-key evil-normal-state-map "@" 'evil-execute-macro)
(define-key evil-normal-state-map "\"" 'evil-use-register)
(define-key evil-normal-state-map "~" 'evil-invert-char)
(define-key evil-normal-state-map "=" 'evil-indent)
(define-key evil-normal-state-map "<" 'evil-shift-left)
(define-key evil-normal-state-map ">" 'evil-shift-right)
(define-key evil-normal-state-map "ZZ" 'evil-save-modified-and-close)
(define-key evil-normal-state-map "ZQ" 'evil-quit)
(define-key evil-normal-state-map (kbd "DEL") 'evil-backward-char)
(define-key evil-normal-state-map [escape] 'evil-force-normal-state)
(define-key evil-normal-state-map [remap cua-paste-pop] 'evil-paste-pop)
(define-key evil-normal-state-map [remap yank-pop] 'evil-paste-pop)

(when (featurep 'tab-bar)
  (define-key evil-normal-state-map "gt" 'tab-bar-switch-to-next-tab)
  (define-key evil-normal-state-map "gT" 'tab-bar-switch-to-prev-tab))

;; go to last change
(define-key evil-normal-state-map "g;" 'goto-last-change)
(define-key evil-normal-state-map "g," 'goto-last-change-reverse)

;; undo
(define-key evil-normal-state-map "u" 'undo)
(define-key evil-normal-state-map "\C-r" 'redo)

;; window commands
(define-prefix-command 'evil-window-map)
(define-key evil-window-map "b" 'evil-window-bottom-right)
(define-key evil-window-map "c" 'evil-window-delete)
(define-key evil-window-map "h" 'evil-window-left)
(define-key evil-window-map "H" 'evil-window-move-far-left)
(define-key evil-window-map "j" 'evil-window-down)
(define-key evil-window-map "J" 'evil-window-move-very-bottom)
(define-key evil-window-map "k" 'evil-window-up)
(define-key evil-window-map "K" 'evil-window-move-very-top)
(define-key evil-window-map "l" 'evil-window-right)
(define-key evil-window-map "L" 'evil-window-move-far-right)
(define-key evil-window-map "n" 'evil-window-new)
(define-key evil-window-map "o" 'delete-other-windows)
(define-key evil-window-map "p" 'evil-window-mru)
(define-key evil-window-map "q" 'evil-quit)
(define-key evil-window-map "r" 'evil-window-rotate-downwards)
(define-key evil-window-map "R" 'evil-window-rotate-upwards)
(define-key evil-window-map "s" 'evil-window-split)
(define-key evil-window-map "S" 'evil-window-split)
(define-key evil-window-map "t" 'evil-window-top-left)
(define-key evil-window-map "v" 'evil-window-vsplit)
(define-key evil-window-map "w" 'evil-window-next)
(define-key evil-window-map "W" 'evil-window-prev)
(define-key evil-window-map "+" 'evil-window-increase-height)
(define-key evil-window-map "-" 'evil-window-decrease-height)
(define-key evil-window-map "_" 'evil-window-set-height)
(define-key evil-window-map "<" 'evil-window-decrease-width)
(define-key evil-window-map ">" 'evil-window-increase-width)
(define-key evil-window-map "=" 'balance-windows)
(define-key evil-window-map "|" 'evil-window-set-width)
(define-key evil-window-map "\C-b" 'evil-window-bottom-right)
(define-key evil-window-map "\C-c" 'evil-window-delete)
(define-key evil-window-map (kbd "C-S-h") 'evil-window-move-far-left)
(define-key evil-window-map (kbd "C-S-j") 'evil-window-move-very-bottom)
(define-key evil-window-map (kbd "C-S-k") 'evil-window-move-very-top)
(define-key evil-window-map (kbd "C-S-l") 'evil-window-move-far-right)
(define-key evil-window-map "\C-n" 'evil-window-new)
(define-key evil-window-map "\C-o" 'delete-other-windows)
(define-key evil-window-map "\C-p" 'evil-window-mru)
(define-key evil-window-map "\C-r" 'evil-window-rotate-downwards)
(define-key evil-window-map (kbd "C-S-r") 'evil-window-rotate-upwards)
(define-key evil-window-map "\C-s" 'evil-window-split)
(define-key evil-window-map (kbd "C-S-s") 'evil-window-split)
(define-key evil-window-map "\C-t" 'evil-window-top-left)
(define-key evil-window-map "\C-v" 'evil-window-vsplit)
(define-key evil-window-map "\C-w" 'evil-window-next)
(define-key evil-window-map (kbd "C-S-W") 'evil-window-prev)
(define-key evil-window-map "\C-_" 'evil-window-set-height)
(define-key evil-window-map "\C-f" 'ffap-other-window)

;;; Motion state

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
(define-key evil-motion-state-map "b" 'evil-backward-word-begin)
(define-key evil-motion-state-map "B" 'evil-backward-WORD-begin)
(define-key evil-motion-state-map "e" 'evil-forward-word-end)
(define-key evil-motion-state-map "E" 'evil-forward-WORD-end)
(define-key evil-motion-state-map "f" 'evil-find-char)
(define-key evil-motion-state-map "F" 'evil-find-char-backward)
(define-key evil-motion-state-map "G" 'evil-goto-line)
(define-key evil-motion-state-map "h" 'evil-backward-char)
(define-key evil-motion-state-map "H" 'evil-window-top)
(define-key evil-motion-state-map "j" 'evil-next-line)
(define-key evil-motion-state-map "k" 'evil-previous-line)
(define-key evil-motion-state-map "l" 'evil-forward-char)
(define-key evil-motion-state-map " " 'evil-forward-char)
(define-key evil-motion-state-map "K" 'evil-lookup)
(define-key evil-motion-state-map "L" 'evil-window-bottom)
(define-key evil-motion-state-map "M" 'evil-window-middle)
(define-key evil-motion-state-map "n" 'evil-search-next)
(define-key evil-motion-state-map "N" 'evil-search-previous)
(define-key evil-motion-state-map "t" 'evil-find-char-to)
(define-key evil-motion-state-map "T" 'evil-find-char-to-backward)
(define-key evil-motion-state-map "w" 'evil-forward-word-begin)
(define-key evil-motion-state-map "W" 'evil-forward-WORD-begin)
(define-key evil-motion-state-map "y" 'evil-yank)
(define-key evil-motion-state-map "Y" 'evil-yank-line)
(define-key evil-motion-state-map "gd" 'evil-goto-definition)
(define-key evil-motion-state-map "ge" 'evil-backward-word-end)
(define-key evil-motion-state-map "gE" 'evil-backward-WORD-end)
(define-key evil-motion-state-map "gg" 'evil-goto-first-line)
(define-key evil-motion-state-map "gj" 'evil-next-visual-line)
(define-key evil-motion-state-map "gk" 'evil-previous-visual-line)
(define-key evil-motion-state-map "g0" 'evil-beginning-of-visual-line)
(define-key evil-motion-state-map "g_" 'evil-last-non-blank)
(define-key evil-motion-state-map "g^" 'evil-first-non-blank-of-visual-line)
(define-key evil-motion-state-map "gm" 'evil-middle-of-visual-line)
(define-key evil-motion-state-map "g$" 'evil-end-of-visual-line)
(define-key evil-motion-state-map "g\C-]" 'evil-jump-to-tag)
(define-key evil-motion-state-map "{" 'evil-backward-paragraph)
(define-key evil-motion-state-map "}" 'evil-forward-paragraph)
(define-key evil-motion-state-map "#" 'evil-search-word-backward)
(define-key evil-motion-state-map "g#" 'evil-search-unbounded-word-backward)
(define-key evil-motion-state-map "$" 'evil-end-of-line)
(define-key evil-motion-state-map "%" 'evil-jump-item)
(define-key evil-motion-state-map "`" 'evil-goto-mark)
(define-key evil-motion-state-map "'" 'evil-goto-mark-line)
(define-key evil-motion-state-map "(" 'evil-backward-sentence-begin)
(define-key evil-motion-state-map ")" 'evil-forward-sentence-begin)
(define-key evil-motion-state-map "]]" 'evil-forward-section-begin)
(define-key evil-motion-state-map "][" 'evil-forward-section-end)
(define-key evil-motion-state-map "[[" 'evil-backward-section-begin)
(define-key evil-motion-state-map "[]" 'evil-backward-section-end)
(define-key evil-motion-state-map "[(" 'evil-previous-open-paren)
(define-key evil-motion-state-map "])" 'evil-next-close-paren)
(define-key evil-motion-state-map "[{" 'evil-previous-open-brace)
(define-key evil-motion-state-map "]}" 'evil-next-close-brace)
(define-key evil-motion-state-map "]s" 'evil-next-flyspell-error)
(define-key evil-motion-state-map "[s" 'evil-prev-flyspell-error)
(define-key evil-motion-state-map "*" 'evil-search-word-forward)
(define-key evil-motion-state-map "g*" 'evil-search-unbounded-word-forward)
(define-key evil-motion-state-map "," 'evil-repeat-find-char-reverse)
(define-key evil-motion-state-map "/" 'evil-search-forward)
(define-key evil-motion-state-map ";" 'evil-repeat-find-char)
(define-key evil-motion-state-map "?" 'evil-search-backward)
(define-key evil-motion-state-map "|" 'evil-goto-column)
(define-key evil-motion-state-map "^" 'evil-first-non-blank)
(define-key evil-motion-state-map "+" 'evil-next-line-first-non-blank)
(define-key evil-motion-state-map "_" 'evil-next-line-1-first-non-blank)
(define-key evil-motion-state-map "-" 'evil-previous-line-first-non-blank)
(define-key evil-motion-state-map "\C-w" 'evil-window-map)
(define-key evil-motion-state-map (kbd "C-6") 'evil-switch-to-windows-last-buffer)
(define-key evil-motion-state-map "\C-]" 'evil-jump-to-tag)
(define-key evil-motion-state-map (kbd "C-b") 'evil-scroll-page-up)
(define-key evil-motion-state-map (kbd "C-e") 'evil-scroll-line-down)
(define-key evil-motion-state-map (kbd "C-f") 'evil-scroll-page-down)
(define-key evil-motion-state-map (kbd "C-o") 'evil-jump-backward)
(define-key evil-motion-state-map (kbd "C-y") 'evil-scroll-line-up)
(define-key evil-motion-state-map (kbd "RET") 'evil-ret)
(define-key evil-motion-state-map "\\" 'evil-execute-in-emacs-state)
(define-key evil-motion-state-map "z^" 'evil-scroll-top-line-to-bottom)
(define-key evil-motion-state-map "z+" 'evil-scroll-bottom-line-to-top)
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
(define-key evil-motion-state-map "v" 'evil-visual-char)
(define-key evil-motion-state-map "V" 'evil-visual-line)
(define-key evil-motion-state-map "\C-v" 'evil-visual-block)
(define-key evil-motion-state-map "gv" 'evil-visual-restore)
(define-key evil-motion-state-map (kbd "C-^") 'evil-buffer)
(define-key evil-motion-state-map [left] 'evil-backward-char)
(define-key evil-motion-state-map [right] 'evil-forward-char)
(define-key evil-motion-state-map [up] 'evil-previous-line)
(define-key evil-motion-state-map [down] 'evil-next-line)
(define-key evil-motion-state-map "zl" 'evil-scroll-column-right)
(define-key evil-motion-state-map [?z right] "zl")
(define-key evil-motion-state-map "zh" 'evil-scroll-column-left)
(define-key evil-motion-state-map [?z left] "zh")
(define-key evil-motion-state-map "zL" 'evil-scroll-right)
(define-key evil-motion-state-map "zH" 'evil-scroll-left)
(define-key evil-motion-state-map
  (read-kbd-macro evil-toggle-key) 'evil-emacs-state)

;; text objects
(define-key evil-outer-text-objects-map "w" 'evil-a-word)
(define-key evil-outer-text-objects-map "W" 'evil-a-WORD)
(define-key evil-outer-text-objects-map "s" 'evil-a-sentence)
(define-key evil-outer-text-objects-map "p" 'evil-a-paragraph)
(define-key evil-outer-text-objects-map "b" 'evil-a-paren)
(define-key evil-outer-text-objects-map "(" 'evil-a-paren)
(define-key evil-outer-text-objects-map ")" 'evil-a-paren)
(define-key evil-outer-text-objects-map "[" 'evil-a-bracket)
(define-key evil-outer-text-objects-map "]" 'evil-a-bracket)
(define-key evil-outer-text-objects-map "B" 'evil-a-curly)
(define-key evil-outer-text-objects-map "{" 'evil-a-curly)
(define-key evil-outer-text-objects-map "}" 'evil-a-curly)
(define-key evil-outer-text-objects-map "<" 'evil-an-angle)
(define-key evil-outer-text-objects-map ">" 'evil-an-angle)
(define-key evil-outer-text-objects-map "'" 'evil-a-single-quote)
(define-key evil-outer-text-objects-map "\"" 'evil-a-double-quote)
(define-key evil-outer-text-objects-map "`" 'evil-a-back-quote)
(define-key evil-outer-text-objects-map "t" 'evil-a-tag)
(define-key evil-outer-text-objects-map "o" 'evil-a-symbol)
(define-key evil-inner-text-objects-map "w" 'evil-inner-word)
(define-key evil-inner-text-objects-map "W" 'evil-inner-WORD)
(define-key evil-inner-text-objects-map "s" 'evil-inner-sentence)
(define-key evil-inner-text-objects-map "p" 'evil-inner-paragraph)
(define-key evil-inner-text-objects-map "b" 'evil-inner-paren)
(define-key evil-inner-text-objects-map "(" 'evil-inner-paren)
(define-key evil-inner-text-objects-map ")" 'evil-inner-paren)
(define-key evil-inner-text-objects-map "[" 'evil-inner-bracket)
(define-key evil-inner-text-objects-map "]" 'evil-inner-bracket)
(define-key evil-inner-text-objects-map "B" 'evil-inner-curly)
(define-key evil-inner-text-objects-map "{" 'evil-inner-curly)
(define-key evil-inner-text-objects-map "}" 'evil-inner-curly)
(define-key evil-inner-text-objects-map "<" 'evil-inner-angle)
(define-key evil-inner-text-objects-map ">" 'evil-inner-angle)
(define-key evil-inner-text-objects-map "'" 'evil-inner-single-quote)
(define-key evil-inner-text-objects-map "\"" 'evil-inner-double-quote)
(define-key evil-inner-text-objects-map "`" 'evil-inner-back-quote)
(define-key evil-inner-text-objects-map "t" 'evil-inner-tag)
(define-key evil-inner-text-objects-map "o" 'evil-inner-symbol)
(define-key evil-motion-state-map "gn" 'evil-next-match)
(define-key evil-motion-state-map "gN" 'evil-previous-match)

(when evil-want-C-i-jump
  (define-key evil-motion-state-map (kbd "C-i") 'evil-jump-forward))

(when evil-want-C-u-scroll
  (define-key evil-motion-state-map (kbd "C-u") 'evil-scroll-up))

(when evil-want-C-d-scroll
  (define-key evil-motion-state-map (kbd "C-d") 'evil-scroll-down))

(when evil-want-C-g-bindings
  (define-key evil-motion-state-map "g\C-g" 'count-words))

;;; Visual state

(define-key evil-visual-state-map "A" 'evil-append)
(define-key evil-visual-state-map "I" 'evil-insert)
(define-key evil-visual-state-map "o" 'exchange-point-and-mark)
(define-key evil-visual-state-map "O" 'evil-visual-exchange-corners)
(define-key evil-visual-state-map "R" 'evil-change)
(define-key evil-visual-state-map "u" 'evil-downcase)
(define-key evil-visual-state-map "U" 'evil-upcase)
(define-key evil-visual-state-map "z=" 'ispell-word)
(define-key evil-visual-state-map "a" evil-outer-text-objects-map)
(define-key evil-visual-state-map "i" evil-inner-text-objects-map)
(define-key evil-visual-state-map (kbd "<insert>") 'undefined)
(define-key evil-visual-state-map (kbd "<insertchar>") 'undefined)
(define-key evil-visual-state-map [remap evil-repeat] 'undefined)
(define-key evil-visual-state-map [escape] 'evil-exit-visual-state)

;;; Operator-Pending state

(define-key evil-operator-state-map "a" evil-outer-text-objects-map)
(define-key evil-operator-state-map "i" evil-inner-text-objects-map)
;; (define-key evil-operator-state-map [escape] 'keyboard-quit)

;;; Insert state

(defvar evil-insert-state-bindings
  `(("\C-v" . quoted-insert)
    ("\C-k" . evil-insert-digraph)
    ("\C-o" . evil-execute-in-normal-state)
    ("\C-r" . evil-paste-from-register)
    ("\C-y" . evil-copy-from-above)
    ("\C-e" . evil-copy-from-below)
    ("\C-n" . evil-complete-next)
    ("\C-p" . evil-complete-previous)
    ("\C-x\C-n" . evil-complete-next-line)
    ("\C-x\C-p" . evil-complete-previous-line)
    ("\C-t" . evil-shift-right-line)
    ("\C-d" . evil-shift-left-line)
    ("\C-a" . evil-paste-last-insertion)
    ([remap delete-backward-char] . evil-delete-backward-char-and-join)
    ,(if evil-want-C-w-delete
         '("\C-w" . evil-delete-backward-word)
       '("\C-w" . evil-window-map))
    ,@(when evil-want-C-u-delete
        '(("\C-u" . evil-delete-back-to-indentation)))
    ([mouse-2] . mouse-yank-primary))
  "Evil's bindings for insert state (for
`evil-insert-state-map'), excluding <delete>, <escape>, and
`evil-toggle-key'.")

(defun evil-update-insert-state-bindings (&optional _option-name remove force)
  "Update bindings in `evil-insert-state-map'.
If no arguments are given add the bindings specified in
`evil-insert-state-bindings'. If REMOVE is non nil, remove only
these bindings. Unless FORCE is non nil, this will not
overwriting existing bindings, which means bindings will not be
added if one already exists for a key and only default bindings
are removed.

Note that <delete>, <escape> and `evil-toggle-key' are not
included in `evil-insert-state-bindings' by default."
  (interactive)
  (dolist (binding evil-insert-state-bindings)
    (cond
     ((and remove
           (or force
               ;; Only remove if the default binding has not changed
               (eq (evil-lookup-key evil-insert-state-map (car binding))
                   (cdr binding))))
      (define-key evil-insert-state-map (car binding) nil))
     ((and (null remove)
           (or force
               ;; Check to see that nothing is bound here before adding
               (not (evil-lookup-key evil-insert-state-map (car binding)))))
      (define-key evil-insert-state-map (car binding) (cdr binding))))))

(define-key evil-insert-state-map [delete] 'delete-char)
(define-key evil-insert-state-map [escape] 'evil-normal-state)
(define-key evil-insert-state-map
  (read-kbd-macro evil-toggle-key) 'evil-emacs-state)

;;; Replace state

(define-key evil-replace-state-map (kbd "DEL") 'evil-replace-backspace)
(define-key evil-replace-state-map [escape] 'evil-normal-state)

;;; Emacs state

(define-key evil-emacs-state-map
  (read-kbd-macro evil-toggle-key) 'evil-exit-emacs-state)

(when evil-want-C-w-in-emacs-state
  (define-key evil-emacs-state-map "\C-w" 'evil-window-map))

;;; Mouse
(define-key evil-motion-state-map [down-mouse-1] 'evil-mouse-drag-region)
(define-key evil-visual-state-map [mouse-2] 'evil-exit-visual-and-repeat)
(define-key evil-normal-state-map [mouse-2] 'mouse-yank-primary)

;; Ex
(define-key evil-motion-state-map ":" 'evil-ex)
(define-key evil-motion-state-map "!" 'evil-shell-command)

(evil-ex-define-cmd "e[dit]" 'evil-edit)
(evil-ex-define-cmd "w[rite]" 'evil-write)
(evil-ex-define-cmd "wa[ll]" 'evil-write-all)
(evil-ex-define-cmd "sav[eas]" 'evil-save)
(evil-ex-define-cmd "r[ead]" 'evil-read)
(evil-ex-define-cmd "b[uffer]" 'evil-buffer)
(evil-ex-define-cmd "bn[ext]" 'evil-next-buffer)
(evil-ex-define-cmd "bp[revious]" 'evil-prev-buffer)
(evil-ex-define-cmd "bN[ext]" "bprevious")
(evil-ex-define-cmd "sb[uffer]" 'evil-split-buffer)
(evil-ex-define-cmd "sbn[ext]" 'evil-split-next-buffer)
(evil-ex-define-cmd "sbp[revious]" 'evil-split-prev-buffer)
(evil-ex-define-cmd "sbN[ext]" "sbprevious")
(evil-ex-define-cmd "buffers" 'buffer-menu)
(evil-ex-define-cmd "files" 'evil-show-files)
(evil-ex-define-cmd "ls" "buffers")

(evil-ex-define-cmd "c[hange]" 'evil-change)
(evil-ex-define-cmd "co[py]" 'evil-copy)
(evil-ex-define-cmd "t" "copy")
(evil-ex-define-cmd "m[ove]" 'evil-move)
(evil-ex-define-cmd "d[elete]" 'evil-ex-delete)
(evil-ex-define-cmd "y[ank]" 'evil-ex-yank)
(evil-ex-define-cmd "go[to]" 'evil-goto-char)
(evil-ex-define-cmd "j[oin]" 'evil-ex-join)
(evil-ex-define-cmd "le[ft]" 'evil-align-left)
(evil-ex-define-cmd "ri[ght]" 'evil-align-right)
(evil-ex-define-cmd "ce[nter]" 'evil-align-center)
(evil-ex-define-cmd "sp[lit]" 'evil-window-split)
(evil-ex-define-cmd "vs[plit]" 'evil-window-vsplit)
(evil-ex-define-cmd "new" 'evil-window-new)
(evil-ex-define-cmd "ene[w]" 'evil-buffer-new)
(evil-ex-define-cmd "vne[w]" 'evil-window-vnew)
(evil-ex-define-cmd "clo[se]" 'evil-window-delete)
(evil-ex-define-cmd "on[ly]" 'delete-other-windows)
(evil-ex-define-cmd "q[uit]" 'evil-quit)
(evil-ex-define-cmd "wq" 'evil-save-and-close)
(evil-ex-define-cmd "quita[ll]" 'evil-quit-all)
(evil-ex-define-cmd "qa[ll]" "quitall")
(evil-ex-define-cmd "cq[uit]" 'evil-quit-all-with-error-code)
(evil-ex-define-cmd "wqa[ll]" 'evil-save-and-quit)
(evil-ex-define-cmd "xa[ll]" "wqall")
(evil-ex-define-cmd "x[it]" 'evil-save-modified-and-close)
(evil-ex-define-cmd "exi[t]" 'evil-save-modified-and-close)
(evil-ex-define-cmd "bd[elete]" 'evil-delete-buffer)
(evil-ex-define-cmd "bw[ipeout]" 'evil-delete-buffer)
(evil-ex-define-cmd "g[lobal]" 'evil-ex-global)
(evil-ex-define-cmd "v[global]" 'evil-ex-global-inverted)
(evil-ex-define-cmd "norm[al]" 'evil-ex-normal)
(evil-ex-define-cmd "s[ubstitute]" 'evil-ex-substitute)
(evil-ex-define-cmd "&" 'evil-ex-repeat-substitute)
(evil-ex-define-cmd "&&" 'evil-ex-repeat-substitute-with-flags)
(evil-ex-define-cmd "~" 'evil-ex-repeat-substitute-with-search)
(evil-ex-define-cmd "~&" 'evil-ex-repeat-substitute-with-search-and-flags)
(evil-ex-define-cmd "registers" 'evil-show-registers)
(evil-ex-define-cmd "marks" 'evil-show-marks)
(evil-ex-define-cmd "delm[arks]" 'evil-delete-marks)
(evil-ex-define-cmd "ju[mps]" 'evil-show-jumps)
(evil-ex-define-cmd "noh[lsearch]" 'evil-ex-nohighlight)
(evil-ex-define-cmd "f[ile]" 'evil-show-file-info)
(evil-ex-define-cmd "<" 'evil-shift-left)
(evil-ex-define-cmd ">" 'evil-shift-right)
(evil-ex-define-cmd "=" 'evil-ex-line-number)
(evil-ex-define-cmd "!" 'evil-shell-command)
(evil-ex-define-cmd "@:" 'evil-ex-repeat)
(evil-ex-define-cmd "mak[e]" 'evil-make)
(evil-ex-define-cmd "cc" 'evil-goto-error)
(evil-ex-define-cmd "cfir[st]" 'first-error)
(evil-ex-define-cmd "cr[ewind]" 'first-error)
(evil-ex-define-cmd "cn[ext]" 'next-error)
(evil-ex-define-cmd "cp[revious]" 'previous-error)
(evil-ex-define-cmd "set-initial-state" 'evil-ex-set-initial-state)
(evil-ex-define-cmd "show-digraphs" 'evil-ex-show-digraphs)
(evil-ex-define-cmd "sor[t]" 'evil-ex-sort)
(evil-ex-define-cmd "res[ize]" 'evil-ex-resize)

(when (featurep 'tab-bar)
  (evil-ex-define-cmd "tabnew" 'tab-bar-new-tab)
  (evil-ex-define-cmd "tabn[ext]" 'tab-bar-switch-to-next-tab)
  (evil-ex-define-cmd "tabp[revious]" 'tab-bar-switch-to-prev-tab))

;; search command line
(define-key evil-ex-search-keymap "\d" #'evil-ex-delete-backward-char)
(define-key evil-ex-search-keymap "\C-f" 'evil-ex-search-command-window)
(define-key evil-ex-search-keymap "\C-r" 'evil-paste-from-register)
(define-key evil-ex-search-keymap "\C-n" 'next-history-element)
(define-key evil-ex-search-keymap "\C-p" 'previous-history-element)

;; ex command line
(define-key evil-ex-completion-map "\d" #'evil-ex-delete-backward-char)
(define-key evil-ex-completion-map "\t" #'evil-ex-completion)
(define-key evil-ex-completion-map [tab] #'evil-ex-completion)
(define-key evil-ex-completion-map [remap completion-at-point] #'evil-ex-completion)
(define-key evil-ex-completion-map "\C-a" 'evil-ex-completion)
(define-key evil-ex-completion-map "\C-b" 'move-beginning-of-line)
(define-key evil-ex-completion-map "\C-c" 'abort-recursive-edit)
(define-key evil-ex-completion-map "\C-d" 'evil-ex-completion)
(define-key evil-ex-completion-map "\C-f" 'evil-ex-command-window)
(define-key evil-ex-completion-map "\C-g" 'abort-recursive-edit)
(define-key evil-ex-completion-map "\C-k" 'evil-insert-digraph)
(define-key evil-ex-completion-map "\C-l" 'evil-ex-completion)
(define-key evil-ex-completion-map "\C-p" #'previous-complete-history-element)
(define-key evil-ex-completion-map "\C-r" 'evil-paste-from-register)
(define-key evil-ex-completion-map "\C-n" #'next-complete-history-element)
(define-key evil-ex-completion-map "\C-u" 'evil-delete-whole-line)
(define-key evil-ex-completion-map "\C-v" #'quoted-insert)
(define-key evil-ex-completion-map "\C-w" 'backward-kill-word)
(define-key evil-ex-completion-map [escape] 'abort-recursive-edit)
(define-key evil-ex-completion-map [S-left] 'backward-word)
(define-key evil-ex-completion-map [S-right] 'forward-word)
(define-key evil-ex-completion-map [up] 'previous-complete-history-element)
(define-key evil-ex-completion-map [down] 'next-complete-history-element)
(define-key evil-ex-completion-map [prior] 'previous-history-element)
(define-key evil-ex-completion-map [next] 'next-history-element)
(define-key evil-ex-completion-map [return] 'exit-minibuffer)
(define-key evil-ex-completion-map (kbd "RET") 'exit-minibuffer)

;; evil-read-key
(define-key evil-read-key-map (kbd "ESC") #'keyboard-quit)
(define-key evil-read-key-map (kbd "C-]") #'keyboard-quit)
(define-key evil-read-key-map (kbd "C-g") #'keyboard-quit)
(define-key evil-read-key-map (kbd "C-q") #'evil-read-quoted-char)
(define-key evil-read-key-map (kbd "C-v") #'evil-read-quoted-char)
(define-key evil-read-key-map (kbd "C-k") #'evil-read-digraph-char)
(define-key evil-read-key-map "\r" "\n")

;; command line window
(evil-define-key 'normal
  evil-command-window-mode-map (kbd "RET") 'evil-command-window-execute)
(evil-define-key 'insert
  evil-command-window-mode-map (kbd "RET") 'evil-command-window-execute)

(provide 'evil-maps)

;;; evil-maps.el ends here
