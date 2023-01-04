;;; evil-commands.el --- Evil commands and operators -*- lexical-binding: t -*-
;; Author: Vegard Øye <vegard_oye at hotmail.com>
;; Maintainer: Vegard Øye <vegard_oye at hotmail.com>

;; Version: 1.15.0

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

(require 'evil-common)
(require 'evil-digraphs)
(require 'evil-search)
(require 'evil-states)
(require 'evil-ex)
(require 'evil-types)
(require 'evil-command-window)
(require 'evil-jumps)
(require 'evil-vars)
(require 'cl-lib)
(require 'reveal)

(declare-function flyspell-overlay-p "flyspell")
(declare-function imenu--in-alist "imenu")

;;; Motions

;; Movement commands, or motions, are defined with the macro
;; `evil-define-motion'. A motion is a command with an optional
;; argument COUNT (interactively accessed by the code "<c>").
;; It may specify the :type command property (e.g., :type line),
;; which determines how it is handled by an operator command.
;; Furthermore, the command must have the command properties
;; :keep-visual t and :repeat motion; these are automatically
;; set by the `evil-define-motion' macro.

;;; Code:

(evil-define-motion evil-forward-char (count &optional crosslines noerror)
  "Move cursor to the right by COUNT characters.
Movement is restricted to the current line unless CROSSLINES is non-nil.
If NOERROR is non-nil, don't signal an error upon reaching the end
of the line or the buffer; just return nil."
  :type exclusive
  (interactive "<c>" (list evil-cross-lines
                           (evil-kbd-macro-suppress-motion-error)))
  (cond
   ((not crosslines)
    ;; For efficiency, narrow the buffer to the projected
    ;; movement before determining the current line
    (evil-with-restriction (point) (+ (point) (or count 1) 1)
      (condition-case err
          (evil-narrow-to-line (forward-char count))
        (error
         ;; Restore the previous command (this one never happened).
         ;; This preserves the current column if the previous command
         ;; was `evil-next-line' or `evil-previous-line'.
         (setq this-command last-command)
         (unless noerror (signal (car err) (cdr err)))))))
   (noerror
    (condition-case nil
        (evil-forward-char count crosslines nil)
      (error)))
   (t
    (evil-motion-loop (nil (or count 1))
      (forward-char)
      ;; don't put the cursor on a newline
      (and (not evil-move-beyond-eol)
           (not (evil-visual-state-p))
           (not (evil-operator-state-p))
           (eolp) (not (eobp)) (not (bolp))
           (forward-char))))))

(evil-define-motion evil-backward-char (count &optional crosslines noerror)
  "Move cursor to the left by COUNT characters.
Movement is restricted to the current line unless CROSSLINES is non-nil.
If NOERROR is non-nil, don't signal an error upon reaching the beginning
of the line or the buffer; just return nil."
  :type exclusive
  (interactive "<c>" (list evil-cross-lines
                           (evil-kbd-macro-suppress-motion-error)))
  (cond
   ((not crosslines)
    ;; Restrict movement to the current line
    (evil-with-restriction (- (point) (or count 1)) (1+ (point))
      (condition-case err
          (evil-narrow-to-line (backward-char count))
        (error
         ;; Restore the previous command (this one never happened).
         ;; This preserves the current column if the previous command
         ;; was `evil-next-line' or `evil-previous-line'.
         (setq this-command last-command)
         (unless noerror (signal (car err) (cdr err)))))))
   (noerror
    (condition-case nil
        (evil-backward-char count crosslines nil)
      (error nil)))
   (t
    (evil-motion-loop (nil (or count 1))
      (backward-char)
      ;; don't put the cursor on a newline
      (unless (or (evil-visual-state-p) (evil-operator-state-p))
        (evil-adjust-cursor))))))

(evil-define-motion evil-next-line (count)
  "Move the cursor COUNT lines down."
  :type line
  (let (line-move-visual)
    (evil-line-move (or count 1))))

(evil-define-motion evil-previous-line (count)
  "Move the cursor COUNT lines up."
  :type line
  (let (line-move-visual)
    (evil-line-move (- (or count 1)))))

(evil-define-motion evil-next-visual-line (count)
  "Move the cursor COUNT screen lines down."
  :type exclusive
  (let ((line-move-visual t))
    (evil-line-move (or count 1))))

(evil-define-motion evil-previous-visual-line (count)
  "Move the cursor COUNT screen lines up."
  :type exclusive
  (let ((line-move-visual t))
    (evil-line-move (- (or count 1)))))

;; used for repeated commands like "dd"
(evil-define-motion evil-line (count)
  "Move COUNT - 1 lines down."
  :type line
  (let (line-move-visual)
    ;; Catch bob and eob errors. These are caused when not moving
    ;; point starting in the first or last line, respectively. In this
    ;; case the current line should be selected.
    (condition-case _err
        (evil-line-move (1- (or count 1)))
      ((beginning-of-buffer end-of-buffer)))))

(evil-define-motion evil-line-or-visual-line (count)
  "Move COUNT - 1 lines down."
  :type screen-line
  (let ((line-move-visual (and evil-respect-visual-line-mode
                               visual-line-mode)))
    ;; Catch bob and eob errors. These are caused when not moving
    ;; point starting in the first or last line, respectively. In this
    ;; case the current line should be selected.
    (condition-case _err
        (evil-line-move (1- (or count 1)))
      ((beginning-of-buffer end-of-buffer)))))

(evil-define-motion evil-beginning-of-line ()
  "Move the cursor to the beginning of the current line."
  :type exclusive
  (move-beginning-of-line nil))

(evil-define-motion evil-end-of-line (count)
  "Move the cursor to the end of the current line.
If COUNT is given, move COUNT - 1 lines downward first."
  :type inclusive
  (move-end-of-line count)
  (when evil-track-eol
    (setq temporary-goal-column most-positive-fixnum
          this-command 'next-line))
  (if (evil-visual-state-p)
      (when evil-v$-excludes-newline
        (let ((evil-move-beyond-eol nil))
          (evil-adjust-cursor)))
    (evil-adjust-cursor)
    (when (eolp)
      ;; prevent "c$" and "d$" from deleting blank lines
      (setq evil-this-type 'exclusive))))

(evil-define-motion evil-beginning-of-visual-line ()
  "Move the cursor to the first character of the current screen line."
  :type exclusive
  (if (fboundp 'beginning-of-visual-line)
      (beginning-of-visual-line)
    (beginning-of-line)))

(evil-define-motion evil-end-of-visual-line (count)
  "Move the cursor to the last character of the current screen line.
If COUNT is given, move COUNT - 1 screen lines downward first."
  :type inclusive
  (if (fboundp 'end-of-visual-line)
      (end-of-visual-line count)
    (end-of-line count)))

(evil-define-motion evil-end-of-line-or-visual-line (count)
  "Move the cursor to the last character of the current screen
line if `visual-line-mode' is active and
`evil-respect-visual-line-mode' is non-nil.  If COUNT is given,
move COUNT - 1 screen lines downward first."
  :type inclusive
  (if (and evil-respect-visual-line-mode
           visual-line-mode)
      (evil-end-of-visual-line count)
    (evil-end-of-line count)))

(evil-define-motion evil-middle-of-visual-line ()
  "Move the cursor to the middle of the current visual line."
  :type exclusive
  (beginning-of-visual-line)
  (evil-with-restriction
      nil
      (save-excursion (end-of-visual-line) (point))
    (move-to-column
     (+ (current-column) -1 (/ (window-body-width) 2)))))

(evil-define-motion evil-percentage-of-line (count)
  "Move the cursor to COUNT % of the width of the current line.
If no COUNT is given, default to 50%."
  :type exclusive
  (let ((line-length (- (line-end-position)
                        (line-beginning-position)
                        (if evil-move-beyond-eol -1 0))))
    (move-to-column (truncate (* line-length (/ (or count 50) 100.0))))))

(evil-define-motion evil-first-non-blank ()
  "Move the cursor to the first non-blank character of the current line."
  :type exclusive
  (evil-narrow-to-line (back-to-indentation)))

(evil-define-motion evil-last-non-blank (count)
  "Move the cursor to the last non-blank character of the current line.
If COUNT is given, move COUNT - 1 lines downward first."
  :type inclusive
  (goto-char
   (save-excursion
     (evil-move-beginning-of-line count)
     (if (re-search-forward "[ \t]*$")
         (max (line-beginning-position)
              (1- (match-beginning 0)))
       (line-beginning-position)))))

(evil-define-motion evil-first-non-blank-of-visual-line ()
  "Move the cursor to the first non blank character
of the current screen line."
  :type exclusive
  (evil-beginning-of-visual-line)
  (skip-chars-forward " \t\r"))

(evil-define-motion evil-next-line-first-non-blank (count)
  "Move the cursor COUNT lines down on the first non-blank character."
  :type line
  (let ((this-command this-command))
    (evil-next-line (or count 1)))
  (evil-first-non-blank))

(evil-define-motion evil-next-line-1-first-non-blank (count)
  "Move the cursor COUNT-1 lines down on the first non-blank character."
  :type line
  (let ((this-command this-command))
    (evil-next-line (1- (or count 1))))
  (evil-first-non-blank))

(evil-define-motion evil-previous-line-first-non-blank (count)
  "Move the cursor COUNT lines up on the first non-blank character."
  :type line
  (let ((this-command this-command))
    (evil-previous-line (or count 1)))
  (evil-first-non-blank))

(evil-define-motion evil-goto-line (count)
  "Go to line COUNT. By default the last line."
  :jump t
  :type line
  (evil-ensure-column
    (if (null count)
        (goto-char (point-max))
      (goto-char (point-min))
      (forward-line (1- count)))))

(evil-define-motion evil-goto-first-line (count)
  "Go to line COUNT. By default the first line."
  :jump t
  :type line
  (evil-goto-line (or count 1)))

(evil-define-motion evil-forward-word-begin (count &optional bigword)
  "Move the cursor to the beginning of the COUNT-th next word.
If BIGWORD is non-nil, move by WORDS.

If this command is called in operator-pending state it behaves
differently. If point reaches the beginning of a word on a new
line point is moved back to the end of the previous line.

If called after a change operator, i.e. cw or cW,
`evil-want-change-word-to-end' is non-nil and point is on a word,
then both behave like ce or cE.

If point is at the end of the buffer and cannot be moved signal
'end-of-buffer is raised.
"
  :type exclusive
  (let ((thing (if bigword 'evil-WORD 'evil-word))
        (orig (point))
        (count (or count 1)))
    (evil-signal-at-bob-or-eob count)
    (cond
     ;; default motion, beginning of next word
     ((not (evil-operator-state-p))
      (evil-forward-beginning thing count))
     ;; the evil-change operator, maybe behave like ce or cE
     ((and evil-want-change-word-to-end
           (memq evil-this-operator evil-change-commands)
           (< orig (or (cdr-safe (bounds-of-thing-at-point thing)) orig)))
      ;; forward-thing moves point to the correct position because
      ;; this is an exclusive motion
      (forward-thing thing count))
     ;; operator state
     (t
      (prog1 (evil-forward-beginning thing count)
        ;; if we reached the beginning of a word on a new line in
        ;; Operator-Pending state, go back to the end of the previous
        ;; line
        (when (and (> (line-beginning-position) orig)
                   (looking-back "^[[:space:]]*" (line-beginning-position)))
          ;; move cursor back as long as the line contains only
          ;; whitespaces and is non-empty
          (evil-move-end-of-line 0)
          ;; skip non-empty lines containing only spaces
          (while (and (looking-back "^[[:space:]]+$" (line-beginning-position))
                      (not (<= (line-beginning-position) orig)))
            (evil-move-end-of-line 0))
          ;; but if the previous line is empty, delete this line
          (when (bolp) (forward-char))))))))

(evil-define-motion evil-forward-word-end (count &optional bigword)
  "Move the cursor to the end of the COUNT-th next word.
If BIGWORD is non-nil, move by WORDS."
  :type inclusive
  (let ((thing (if bigword 'evil-WORD 'evil-word))
        (count (or count 1)))
    (evil-signal-at-bob-or-eob count)
    ;; Evil special behaviour: e or E on a one-character word in
    ;; operator state does not move point
    (unless (and (evil-operator-state-p)
                 (= 1 count)
                 (let ((bnd (bounds-of-thing-at-point thing)))
                   (and bnd
                        (= (car bnd) (point))
                        (= (cdr bnd) (1+ (point)))))
                 (looking-at "[[:word:]]"))
      (evil-forward-end thing count))))

(evil-define-motion evil-backward-word-begin (count &optional bigword)
  "Move the cursor to the beginning of the COUNT-th previous word.
If BIGWORD is non-nil, move by WORDS."
  :type exclusive
  (let ((thing (if bigword 'evil-WORD 'evil-word)))
    (evil-signal-at-bob-or-eob (- (or count 1)))
    (evil-backward-beginning thing count)))

(evil-define-motion evil-backward-word-end (count &optional bigword)
  "Move the cursor to the end of the COUNT-th previous word.
If BIGWORD is non-nil, move by WORDS."
  :type inclusive
  (let ((thing (if bigword 'evil-WORD 'evil-word)))
    (evil-signal-at-bob-or-eob (- (or count 1)))
    (evil-backward-end thing count)))

(evil-define-motion evil-forward-WORD-begin (count)
  "Move the cursor to the beginning of the COUNT-th next WORD."
  :type exclusive
  (evil-forward-word-begin count t))

(evil-define-motion evil-forward-WORD-end (count)
  "Move the cursor to the end of the COUNT-th next WORD."
  :type inclusive
  (evil-forward-word-end count t))

(evil-define-motion evil-backward-WORD-begin (count)
  "Move the cursor to the beginning of the COUNT-th previous WORD."
  :type exclusive
  (evil-backward-word-begin count t))

(evil-define-motion evil-backward-WORD-end (count)
  "Move the cursor to the end of the COUNT-th previous WORD."
  :type inclusive
  (evil-backward-word-end count t))

;; section movement
(evil-define-motion evil-forward-section-begin (count)
  "Move the cursor to the beginning of the COUNT-th next section."
  :jump t
  :type exclusive
  (evil-signal-at-bob-or-eob count)
  (evil-forward-beginning 'evil-defun count))

(evil-define-motion evil-forward-section-end (count)
  "Move the cursor to the end of the COUNT-th next section."
  :jump t
  :type inclusive
  (evil-signal-at-bob-or-eob count)
  (evil-forward-end 'evil-defun count)
  (unless (eobp) (forward-line)))

(evil-define-motion evil-backward-section-begin (count)
  "Move the cursor to the beginning of the COUNT-th previous section."
  :jump t
  :type exclusive
  (evil-signal-at-bob-or-eob (- (or count 1)))
  (evil-backward-beginning 'evil-defun count))

(evil-define-motion evil-backward-section-end (count)
  "Move the cursor to the end of the COUNT-th previous section."
  :jump t
  :type inclusive
  (evil-signal-at-bob-or-eob (- (or count 1)))
  (end-of-line -1)
  (evil-backward-end 'evil-defun count)
  (unless (eobp) (forward-line)))

(evil-define-motion evil-forward-sentence-begin (count)
  "Move to the next COUNT-th beginning of a sentence or end of a paragraph."
  :jump t
  :type exclusive
  (evil-signal-at-bob-or-eob count)
  (evil-forward-nearest count
                        #'(lambda (_cnt)
                            (evil-forward-beginning 'evil-sentence))
                        #'evil-forward-paragraph))

(evil-define-motion evil-backward-sentence-begin (count)
  "Move to the previous COUNT-th beginning of a sentence or paragraph."
  :jump t
  :type exclusive
  (evil-signal-at-bob-or-eob (- (or count 1)))
  (evil-forward-nearest (- (or count 1))
                        #'(lambda (_cnt)
                            (evil-backward-beginning 'evil-sentence))
                        #'(lambda (_cnt)
                            (evil-backward-paragraph))))

(evil-define-motion evil-forward-paragraph (count)
  "Move to the end of the COUNT-th next paragraph."
  :jump t
  :type exclusive
  (evil-signal-at-bob-or-eob count)
  (evil-forward-end 'evil-paragraph count)
  (unless (eobp) (forward-line)))

(evil-define-motion evil-backward-paragraph (count)
  "Move to the beginning of the COUNT-th previous paragraph."
  :jump t
  :type exclusive
  (evil-signal-at-bob-or-eob (- (or count 1)))
  (unless (eobp) (forward-line))
  (evil-backward-beginning 'evil-paragraph count)
  (unless (bobp) (forward-line -1)))

(defvar hif-ifx-else-endif-regexp)
(evil-define-motion evil-jump-item (count)
  "Find the next item in this line after or under the cursor
and jump to the corresponding one."
  :jump t
  :type inclusive
  (cond
   ;; COUNT% jumps to a line COUNT percentage down the file
   (count
    (evil-ensure-column
      (goto-char
       (let ((size (- (point-max) (point-min))))
         (+ (point-min)
            (if (> size 80000)
                (* count (/ size 100))
              (/ (* count size) 100))))))
    (setq evil-this-type 'line))
   ((and (evil-looking-at-start-comment t)
         (let ((pnt (point)))
           (forward-comment 1)
           (or (not (bolp))
               (prog1 nil (goto-char pnt)))))
    (backward-char))
   ((and (not (eolp)) (evil-looking-at-end-comment t))
    (forward-comment -1))
   ((and
     (memq major-mode '(c-mode c++-mode))
     (require 'hideif nil t)
     (with-no-warnings
       (let* ((hif-else-regexp (concat hif-cpp-prefix "\\(?:else\\|elif[ \t]+\\)"))
              (hif-ifx-else-endif-regexp
               (concat hif-ifx-regexp "\\|" hif-else-regexp "\\|" hif-endif-regexp)))
         (cond
          ((save-excursion (beginning-of-line) (or (hif-looking-at-ifX) (hif-looking-at-else)))
           (hif-find-next-relevant)
           (while (hif-looking-at-ifX)
             (hif-ifdef-to-endif)
             (hif-find-next-relevant))
           t)
          ((save-excursion (beginning-of-line) (hif-looking-at-endif))
           (hif-endif-to-ifdef)
           t))))))
   (t
    (let* ((open (point-max))
           (close (point-max))
           (open-pair (condition-case nil
                          (save-excursion
                            ;; consider the character right before eol given that
                            ;; point may be placed there, e.g. in visual state
                            (when (and (eolp) (not (bolp)))
                              (backward-char))
                            (setq open (1- (scan-lists (point) 1 -1)))
                            (when (< open (line-end-position))
                              (goto-char open)
                              (forward-list)
                              (1- (point))))
                        (error nil)))
           (close-pair (condition-case nil
                           (save-excursion
                             ;; consider the character right before eol given that
                             ;; point may be placed there, e.g. in visual state
                             (when (and (eolp) (not (bolp)))
                               (backward-char))
                             (setq close (1- (scan-lists (point) 1 1)))
                             (when (< close (line-end-position))
                               (goto-char (1+ close))
                               (backward-list)
                               (point)))
                         (error nil))))
      (cond
       ((not (or open-pair close-pair))
        ;; nothing found, check if we are inside a string
        (let ((pnt (point))
              (bnd (bounds-of-thing-at-point 'evil-string)))
          (if (not (and bnd (< (point) (cdr bnd))))
              ;; no, then we really failed
              (user-error "No matching item found on the current line")
            ;; yes, go to the end of the string and try again
            (let ((endstr (cdr bnd)))
              (when (or (save-excursion
                          (goto-char endstr)
                          (let ((b (bounds-of-thing-at-point 'evil-string)))
                            (and b (< (point) (cdr b))))) ; not at end of string
                        (condition-case nil
                            (progn
                              (goto-char endstr)
                              (evil-jump-item)
                              nil)
                          (error t)))
                ;; failed again, go back to original point
                (goto-char pnt)
                (user-error "No matching item found on the current line"))))))
       ((< open close) (goto-char open-pair))
       (t (goto-char close-pair)))))))

(evil-define-motion evil-next-flyspell-error (count)
  "Go to the COUNT'th spelling mistake after point."
  :jump t
  (unless (bound-and-true-p flyspell-mode) (signal 'search-failed nil))
  (let ((fwd (> (or count 1) 0)) (start (point)) (pos (point)) ov)
    (dotimes (_ (abs (or count 1)))
      (let ((limit (if fwd (point-max) (point-min))) wrappedp)
        (when fwd (setq pos (save-excursion (goto-char pos)
                                            (skip-syntax-forward "w") (point))))
        (while (progn (if (if fwd (>= pos limit) (<= pos limit))
                          (if (or wrappedp (not evil-search-wrap))
                              (signal 'search-failed nil)
                            (setq wrappedp t
                                  limit start
                                  pos (if fwd (point-min)
                                        (previous-overlay-change (point-max)))))
                        (setq pos (if fwd (next-overlay-change pos)
                                    (previous-overlay-change pos))))
                      (not (setq ov (seq-find #'flyspell-overlay-p
                                              (overlays-at pos))))))
        (when wrappedp (let (message-log-max) (message "Search wrapped")))))
    (goto-char (overlay-start ov))))

(evil-define-motion evil-prev-flyspell-error (count)
  "Go to the COUNT'th spelling mistake preceding point."
  :jump t
  (evil-next-flyspell-error (- (or count 1))))

(evil-define-motion evil-previous-open-paren (count)
  "Go to [count] previous unmatched '('."
  :type exclusive
  (evil-up-paren ?\( ?\) (- (or count 1))))

(evil-define-motion evil-next-close-paren (count)
  "Go to [count] next unmatched ')'."
  :type exclusive
  (forward-char)
  (evil-up-paren ?\( ?\) (or count 1))
  (backward-char))

(evil-define-motion evil-previous-open-brace (count)
  "Go to [count] previous unmatched '{'."
  :type exclusive
  (evil-up-paren ?{ ?} (- (or count 1))))

(evil-define-motion evil-next-close-brace (count)
  "Go to [count] next unmatched '}'."
  :type exclusive
  (forward-char)
  (evil-up-paren ?{ ?} (or count 1))
  (backward-char))

(defun evil--lowercase-markers ()
  "Get all lowercase markers."
  (cl-remove-if-not (lambda (x) (and (markerp (cdr x))
                                     (<= ?a (car x) ?z)))
                    evil-markers-alist))

(defun evil--next-mark (forwardp)
  "Move to next lowercase mark.
Move forward if FORWARDP is truthy or backward if falsey.
Loop back to the top of buffer if the end is reached."
  (let ((pos (point))
        (sorted-markers (sort (evil--lowercase-markers)
                              (lambda (a b) (< (cdr a) (cdr b))))))
    (cond
     ((null sorted-markers)
      (user-error "No marks in this buffer"))
     (forwardp
      (let ((next-marker (cl-some (lambda (x) (and (< pos (cdr x)) (cdr x)))
                                  sorted-markers)))
        (if next-marker
            (goto-char (marker-position next-marker))
          (goto-char (marker-position (cdar sorted-markers))))))
     (t
      (let* ((descending-markers (reverse sorted-markers))
             (prev-marker (cl-some (lambda (x) (and (> pos (cdr x)) (cdr x)))
                                   descending-markers)))
        (if prev-marker
            (goto-char (marker-position prev-marker))
          (goto-char (marker-position (cdar descending-markers)))))))))

(evil-define-motion evil-next-mark (count)
  "Go to [count] next lowercase mark."
  :keep-visual t
  :repeat nil
  :type exclusive
  :jump t
  (dotimes (_ (or count 1))
    (evil--next-mark t)))

(evil-define-motion evil-next-mark-line (count)
  "Go to [count] line of next lowercase mark after current line."
  :keep-visual t
  :repeat nil
  :type exclusive
  :jump t
  (if (evil--lowercase-markers)
      (dotimes (_ (or count 1))
        (evil-end-of-line)
        (evil--next-mark t)
        (evil-first-non-blank))
    (user-error "No marks in this buffer")))

(evil-define-motion evil-previous-mark (count)
  "Go to [count] previous lowercase mark."
  :keep-visual t
  :repeat nil
  :type exclusive
  :jump t
  (dotimes (_ (or count 1))
    (evil--next-mark nil)))

(evil-define-motion evil-previous-mark-line (count)
  "Go to [count] line of previous lowercase mark before current line."
  :keep-visual t
  :repeat nil
  :type exclusive
  :jump t
  (if (evil--lowercase-markers)
      (dotimes (_ (or count 1))
        (evil-beginning-of-line)
        (evil--next-mark nil)
        (evil-first-non-blank))
    (user-error "No marks in this buffer")))

(evil-define-command evil-set-col-0-mark (beg end mark)
  "Set MARK at column 0 of line of END. Default is cursor line."
  (interactive "<r><a>")
  (if (< 1 (length mark))
      (user-error "Trailing characters")
    (save-excursion
      (goto-char (if (eobp) end (1- end)))
      (evil-beginning-of-line)
      (evil-set-marker (string-to-char mark)))))

(evil-define-motion evil-find-char (count char)
  "Move to the next COUNT'th occurrence of CHAR.
Movement is restricted to the current line unless `evil-cross-lines' is non-nil."
  :type inclusive
  (interactive "<c><C>")
  (setq count (or count 1))
  (let ((fwd (> count 0))
        (visual (and evil-respect-visual-line-mode
                     visual-line-mode)))
    (setq evil-last-find (list #'evil-find-char char fwd))
    (when fwd (evil-forward-char 1 evil-cross-lines))
    (let ((case-fold-search nil))
      (unless (prog1
                  (search-forward (char-to-string char)
                                  (cond (evil-cross-lines
                                         nil)
                                        ((and fwd visual)
                                         (save-excursion
                                           (end-of-visual-line)
                                           (point)))
                                        (fwd
                                         (line-end-position))
                                        (visual
                                         (save-excursion
                                           (beginning-of-visual-line)
                                           (point)))
                                        (t
                                         (line-beginning-position)))
                                  t count)
                (when fwd (backward-char)))
        (user-error "Can't find %c" char)))))

(evil-define-motion evil-find-char-backward (count char)
  "Move to the previous COUNT'th occurrence of CHAR."
  :type exclusive
  (interactive "<c><C>")
  (evil-find-char (- (or count 1)) char))

(evil-define-motion evil-find-char-to (count char)
  "Move before the next COUNT'th occurrence of CHAR."
  :type inclusive
  (interactive "<c><C>")
  (unwind-protect
      (progn
        (evil-find-char count char)
        (if (> (or count 1) 0)
            (backward-char)
          (forward-char)))
    (setcar evil-last-find #'evil-find-char-to)))

(evil-define-motion evil-find-char-to-backward (count char)
  "Move before the previous COUNT'th occurrence of CHAR."
  :type exclusive
  (interactive "<c><C>")
  (evil-find-char-to (- (or count 1)) char))

(evil-define-motion evil-repeat-find-char (count)
  "Repeat the last find COUNT times."
  :type inclusive
  (setq count (or count 1))
  (if evil-last-find
      (let ((cmd (car evil-last-find))
            (char (nth 1 evil-last-find))
            (fwd (nth 2 evil-last-find))
            evil-last-find)
        ;; ensure count is non-negative
        (when (< count 0)
          (setq count (- count)
                fwd (not fwd)))
        ;; skip next character when repeating t or T
        (and (eq cmd #'evil-find-char-to)
             evil-repeat-find-to-skip-next
             (= count 1)
             (or (and fwd (= (char-after (1+ (point))) char))
                 (and (not fwd) (= (char-before) char)))
             (setq count (1+ count)))
        (funcall cmd (if fwd count (- count)) char)
        (unless (nth 2 evil-last-find)
          (setq evil-this-type 'exclusive)))
    (user-error "No previous search")))

(evil-define-motion evil-repeat-find-char-reverse (count)
  "Repeat the last find COUNT times in the opposite direction."
  :type inclusive
  (evil-repeat-find-char (- (or count 1))))

;; ceci n'est pas une pipe
(evil-define-motion evil-goto-column (count)
  "Go to column COUNT on the current line.
Columns are counted from zero."
  :type exclusive
  (move-to-column (or count 0)))

(evil-define-command evil-goto-mark (char &optional noerror)
  "Go to the marker specified by CHAR."
  :keep-visual t
  :repeat nil
  :type exclusive
  :jump t
  (interactive (list (read-char)))
  (let ((marker (evil-get-marker char)))
    (cond
     ((markerp marker)
      (switch-to-buffer (marker-buffer marker))
      (goto-char (marker-position marker)))
     ((numberp marker)
      (goto-char marker))
     ((consp marker)
      (when (or (find-buffer-visiting (car marker))
                (and (y-or-n-p (format "Visit file %s again? "
                                       (car marker)))
                     (find-file (car marker))))
        (goto-char (cdr marker))))
     ((not noerror)
      (user-error "Marker `%c' is not set%s" char
                  (if (evil-global-marker-p char) ""
                    " in this buffer"))))))

(evil-define-command evil-goto-mark-line (char &optional noerror)
  "Go to the line of the marker specified by CHAR."
  :keep-visual t
  :repeat nil
  :type line
  :jump t
  (interactive (list (read-char)))
  (evil-goto-mark char noerror)
  (evil-first-non-blank))

(evil-define-motion evil-goto-last-change (count)
  "Like `goto-last-change' but takes a COUNT rather than a span."
  (eval-and-compile (require 'goto-chg))
  (setq this-command 'goto-last-change)
  (dotimes (_ (or count 1))
    (goto-last-change nil)))

(evil-define-motion evil-goto-last-change-reverse (count)
  "Like `goto-last-change-reverse' but takes a COUNT rather than a span."
  (eval-and-compile (require 'goto-chg))
  (setq this-command 'goto-last-change-reverse)
  (dotimes (_ (or count 1))
    (goto-last-change-reverse nil)))

(evil-define-motion evil-jump-backward (count)
  "Go to older position in jump list.
To go the other way, press \
\\<evil-motion-state-map>\\[evil-jump-forward]."
  (evil--jump-backward count))

(evil-define-motion evil-jump-forward (count)
  "Go to newer position in jump list.
To go the other way, press \
\\<evil-motion-state-map>\\[evil-jump-backward]."
  (evil--jump-forward count))

(evil-define-motion evil-jump-backward-swap (count)
  "Go to the previous position in jump list.
The current position is placed in the jump list."
  (let ((pnt (point)))
    (evil--jump-backward 1)
    (evil-set-jump pnt)))

(defvar xref-prompt-for-identifier)
(evil-define-motion evil-jump-to-tag (arg)
  "Jump to tag under point.
If called with a prefix argument, provide a prompt
for specifying the tag."
  :jump t
  (interactive "P")
  (cond
   ((fboundp 'xref-find-definitions)
    (let ((xref-prompt-for-identifier arg))
      (call-interactively #'xref-find-definitions)))
   ((fboundp 'find-tag)
    (if arg (call-interactively #'find-tag)
      (let ((tag (funcall (or find-tag-default-function
                              (get major-mode 'find-tag-default-function)
                              #'find-tag-default))))
        (unless tag (user-error "No tag candidate found around point"))
        (find-tag tag))))))

(evil-define-motion evil-lookup ()
  "Look up the keyword at point.
Calls `evil-lookup-func'."
  (funcall evil-lookup-func))

(defun evil-ret-gen (count indent?)
  (let* ((field  (get-char-property (point) 'field))
         (button (get-char-property (point) 'button))
         (doc    (get-char-property (point) 'widget-doc))
         (widget (or field button doc)))
    (cond
     ((and widget
           (fboundp 'widget-type)
           (fboundp 'widget-button-press)
           (or (and (symbolp widget)
                    (get widget 'widget-type))
               (and (consp widget)
                    (get (widget-type widget) 'widget-type))))
      (when (evil-operator-state-p)
        (setq evil-inhibit-operator t))
      (when (fboundp 'widget-button-press)
        (widget-button-press (point))))
     ((and (fboundp 'button-at)
           (fboundp 'push-button)
           (button-at (point)))
      (when (evil-operator-state-p)
        (setq evil-inhibit-operator t))
      (push-button))
     ((or (evil-emacs-state-p)
          (and (evil-insert-state-p)
               (not buffer-read-only)))
      (if (not indent?)
          (newline count)
        (delete-horizontal-space t)
        (newline count)
        (indent-according-to-mode)))
     (t
      (evil-next-line-first-non-blank count)))))

(evil-define-motion evil-ret (count)
  "Move the cursor COUNT lines down.
If point is on a widget or a button, click on it.
In Insert state, insert a newline."
  :type line
  (evil-ret-gen count nil))

(evil-define-motion evil-ret-and-indent (count)
  "Move the cursor COUNT lines down.
If point is on a widget or a button, click on it.
In Insert state, insert a newline and indent."
  :type line
  (evil-ret-gen count t))

(evil-define-motion evil-window-top (count)
  "Move the cursor to line COUNT from the top of the window."
  :jump t
  :type line
  (evil-ensure-column
    (move-to-window-line (max (or count 0)
                              (if (= (point-min) (window-start))
                                  0
                                scroll-margin)))))

(evil-define-motion evil-window-middle ()
  "Move the cursor to the middle line in the window."
  :jump t
  :type line
  (evil-ensure-column (move-to-window-line nil)))

(evil-define-motion evil-window-bottom (count)
  "Move the cursor to line COUNT from the bottom of the window."
  :jump t
  :type line
  (evil-ensure-column
    (move-to-window-line (- (max (or count 1) (1+ scroll-margin))))))

;; scrolling
(evil-define-command evil-scroll-line-up (count)
  "Scroll the window COUNT lines upwards."
  :repeat nil
  :keep-visual t
  (interactive "p")
  (let ((scroll-preserve-screen-position nil))
    (scroll-down count)))

(evil-define-command evil-scroll-line-down (count)
  "Scroll the window COUNT lines downwards."
  :repeat nil
  :keep-visual t
  (interactive "p")
  (let ((scroll-preserve-screen-position nil))
    (scroll-up count)))

(evil-define-command evil-scroll-count-reset ()
  "Set `evil-scroll-count' to 0.
`evil-scroll-up' and `evil-scroll-down' will then scroll by half of
the screen (the default)."
  :repeat nil
  :keep-visual t
  (interactive)
  (setq evil-scroll-count 0))

;; With `scroll-preserve-screen-position' `scroll-up'/`scroll-down'
;; target the same cursor pixel Y-coordinate while `last-command' has
;; the `scroll-command' property. However the target needs updating
;; when e.g. scrolling maximally to the first or last line and then
;; switching scroll direction. Do this by resetting `last-command'
;; when `real-this-command' (`evil-ensure-column' modifies
;; `this-command') changed from last value:

(evil-define-command evil-scroll-up (count)
  "Scroll the window and the cursor COUNT lines upwards.
If COUNT is not specified the function scrolls up `evil-scroll-count'
lines, which is the last used count.
If the scroll count is zero the command scrolls half the screen."
  :repeat nil
  :keep-visual t
  (interactive "<c>")
  (when (= (line-beginning-position) (point-min))
    (signal 'beginning-of-buffer nil))
  (setq count (or count (max 0 evil-scroll-count))
        evil-scroll-count count)
  (when (zerop count) (setq count (/ (window-body-height) 2)))
  (evil-ensure-column
    (let ((opoint (point)))
      (condition-case nil
          (let ((scroll-preserve-screen-position 'always)
                (last-command (when (eq real-last-command real-this-command)
                                real-last-command)))
            (scroll-down count))
        (:success
         ;; Redo if `scroll-down' only did partial scroll up to BOB
         (when (<= (window-start) (point-min))
           (goto-char opoint)
           (vertical-motion (- count))))
        (beginning-of-buffer (vertical-motion (- count)))))))
(put 'evil-scroll-up 'scroll-command t)

(evil-define-command evil-scroll-down (count)
  "Scroll the window and the cursor COUNT lines downwards.
If COUNT is not specified the function scrolls down
`evil-scroll-count' lines, which is the last used count.
If the scroll count is zero the command scrolls half the screen."
  :repeat nil
  :keep-visual t
  (interactive "<c>")
  (when (= (line-end-position) (point-max))
    (signal 'end-of-buffer nil))
  (setq count (or count (max 0 evil-scroll-count))
        evil-scroll-count count)
  (when (zerop count) (setq count (/ (window-body-height) 2)))
  (evil-ensure-column
    ;; BUG #660: First check whether the eob is visible.
    ;; In that case we do not scroll but merely move point.
    (if (<= (point-max) (window-end))
        (vertical-motion count)
      (condition-case nil
          (let ((scroll-preserve-screen-position 'always)
                (last-command (when (eq real-last-command real-this-command)
                                real-last-command)))
            (scroll-up count))
        (:success
         ;; If EOB became visible: Scroll it to the bottom
         (save-excursion
           (goto-char (window-start))
           (vertical-motion (max 0 (- (window-height) 1 scroll-margin)))
           (when (<= (point-max) (point)) (recenter -1))))
        (end-of-buffer (goto-char (point-max)) (recenter -1))))))
(put 'evil-scroll-down 'scroll-command t)

(evil-define-command evil-scroll-page-up (count)
  "Scroll the window COUNT pages upwards."
  :repeat nil
  :keep-visual t
  (interactive "p")
  (evil-ensure-column
    (dotimes (i count)
      (condition-case err
          (scroll-down nil)
        (beginning-of-buffer
         (if (and (bobp) (zerop i))
             (signal (car err) (cdr err))
           (goto-char (point-min))))))))

(evil-define-command evil-scroll-page-down (count)
  "Scroll the window COUNT pages downwards."
  :repeat nil
  :keep-visual t
  (interactive "p")
  (evil-ensure-column
    (dotimes (i count)
      (condition-case err
          (scroll-up nil)
        (end-of-buffer
         (if (and (eobp) (zerop i))
             (signal (car err) (cdr err))
           (goto-char (point-max))))))))

(evil-define-command evil-scroll-line-to-top (count)
  "Scroll line number COUNT (or the cursor line) to the top of the window."
  :repeat nil
  :keep-visual t
  (interactive "<c>")
  (when count
    (evil-save-column
      (goto-char (point-min))
      (forward-line (1- count))))
  (recenter 0))

(evil-define-command evil-scroll-line-to-top-first-non-blank (count)
  "Scroll line number COUNT (or the cursor line) to the top of the window.
Then move the cursor to the first non-blank character of that line."
  :repeat nil
  :keep-visual t
  (interactive "<c>")
  (evil-scroll-line-to-top count)
  (evil-first-non-blank))

(evil-define-command evil-scroll-line-to-center (count)
  "Scroll line number COUNT (or the cursor line) to the center of the window."
  :repeat nil
  :keep-visual t
  (interactive "<c>")
  (when count
    (evil-save-column
      (goto-char (point-min))
      (forward-line (1- count))))
  (recenter))

(evil-define-command evil-scroll-line-to-center-first-non-blank (count)
  "Scroll line number COUNT (or the cursor line) to the center of the window.
Then move the cursor to the first non-blank character of that line."
  :repeat nil
  :keep-visual t
  (interactive "<c>")
  (evil-scroll-line-to-center count)
  (evil-first-non-blank))

(evil-define-command evil-scroll-line-to-bottom (count)
  "Scroll line number COUNT (or the cursor line) to the bottom of the window."
  :repeat nil
  :keep-visual t
  (interactive "<c>")
  (when count
    (evil-save-column
      (goto-char (point-min))
      (forward-line (1- count))))
  (recenter -1))

(evil-define-command evil-scroll-line-to-bottom-first-non-blank (count)
  "Scroll line number COUNT (or the cursor line) to the bottom of the window.
Then move the cursor to the first non-blank character of that line."
  :repeat nil
  :keep-visual t
  (interactive "<c>")
  (evil-scroll-line-to-bottom count)
  (evil-first-non-blank))

(evil-define-command evil-scroll-bottom-line-to-top (count)
  "Scroll the line right below the window,
or line COUNT to the top of the window."
  :repeat nil
  :keep-visual t
  (interactive "<c>")
  (unless count
    (goto-char (window-end))
    (evil-first-non-blank))
  (evil-scroll-line-to-top count))

(evil-define-command evil-scroll-top-line-to-bottom (count)
  "Scroll the line right above the window,
or line COUNT to the bottom of the window."
  :repeat nil
  :keep-visual t
  (interactive "<c>")
  (unless count
    (goto-char (window-start))
    (evil-first-non-blank))
  (evil-scroll-line-to-bottom count))

(evil-define-command evil-scroll-left (count)
  "Scroll the window COUNT half-screenwidths to the left."
  :repeat nil
  :keep-visual t
  (interactive "p")
  (evil-with-hproject-point-on-window
    (scroll-right (* count (/ (window-width) 2)))))

(evil-define-command evil-scroll-right (count)
  "Scroll the window COUNT half-screenwidths to the right."
  :repeat nil
  :keep-visual t
  (interactive "p")
  (evil-with-hproject-point-on-window
    (scroll-left (* count (/ (window-width) 2)))))

(evil-define-command evil-scroll-column-left (count)
  "Scroll the window COUNT columns to the left."
  :repeat nil
  :keep-visual t
  (interactive "p")
  (evil-with-hproject-point-on-window
    (scroll-right count)))

(evil-define-command evil-scroll-column-right (count)
  "Scroll the window COUNT columns to the right."
  :repeat nil
  :keep-visual t
  (interactive "p")
  (evil-with-hproject-point-on-window
    (scroll-left count)))

;;; Text objects

;; Text objects are defined with `evil-define-text-object'. In Visual
;; state, they modify the current selection; in Operator-Pending
;; state, they return a pair of buffer positions. Outer text objects
;; are bound in the keymap `evil-outer-text-objects-map', and inner
;; text objects are bound in `evil-inner-text-objects-map'.
;;
;; Common text objects like words, WORDS, paragraphs and sentences are
;; defined via a corresponding move-function. This function must have
;; the following properties:
;;
;;   1. Take exactly one argument, the count.
;;   2. When the count is positive, move point forward to the first
;;      character after the end of the next count-th object.
;;   3. When the count is negative, move point backward to the first
;;      character of the count-th previous object.
;;   4. If point is placed on the first character of an object, the
;;      backward motion does NOT count that object.
;;   5. If point is placed on the last character of an object, the
;;      forward motion DOES count that object.
;;   6. The return value is "count left", i.e., in forward direction
;;      count is decreased by one for each successful move and in
;;      backward direction count is increased by one for each
;;      successful move, returning the final value of count.
;;      Therefore, if the complete move is successful, the return
;;      value is 0.
;;
;; A useful macro in this regard is `evil-motion-loop', which quits
;; when point does not move further and returns the count difference.
;; It also provides a "unit value" of 1 or -1 for use in each
;; iteration. For example, a hypothetical "foo-bar" move could be
;; written as such:
;;
;;     (defun foo-bar (count)
;;       (evil-motion-loop (var count)
;;         (forward-foo var) ; `var' is 1 or -1 depending on COUNT
;;         (forward-bar var)))
;;
;; If "forward-foo" and "-bar" didn't accept negative arguments,
;; we could choose their backward equivalents by inspecting `var':
;;
;;     (defun foo-bar (count)
;;       (evil-motion-loop (var count)
;;         (cond
;;          ((< var 0)
;;           (backward-foo 1)
;;           (backward-bar 1))
;;          (t
;;           (forward-foo 1)
;;           (forward-bar 1)))))
;;
;; After a forward motion, point has to be placed on the first
;; character after some object, unless no motion was possible at all.
;; Similarly, after a backward motion, point has to be placed on the
;; first character of some object. This implies that point should
;; NEVER be moved to eob or bob, unless an object ends or begins at
;; eob or bob. (Usually, Emacs motions always move as far as possible.
;; But we want to use the motion-function to identify certain objects
;; in the buffer, and thus exact movement to object boundaries is
;; required.)

(evil-define-text-object evil-a-word (count &optional beg end type)
  "Select a word."
  (evil-select-a-restricted-object 'evil-word beg end type count))

(evil-define-text-object evil-inner-word (count &optional beg end type)
  "Select inner word."
  (evil-select-inner-restricted-object 'evil-word beg end type count))

(evil-define-text-object evil-a-WORD (count &optional beg end type)
  "Select a WORD."
  (evil-select-a-restricted-object 'evil-WORD beg end type count))

(evil-define-text-object evil-inner-WORD (count &optional beg end type)
  "Select inner WORD."
  (evil-select-inner-restricted-object 'evil-WORD beg end type count))

(evil-define-text-object evil-a-symbol (count &optional beg end type)
  "Select a symbol."
  (evil-select-an-object 'evil-symbol beg end type count))

(evil-define-text-object evil-inner-symbol (count &optional beg end type)
  "Select inner symbol."
  (evil-select-inner-object 'evil-symbol beg end type count))

(evil-define-text-object evil-a-sentence (count &optional beg end type)
  "Select a sentence."
  (evil-select-an-object 'evil-sentence beg end type count))

(evil-define-text-object evil-inner-sentence (count &optional beg end type)
  "Select inner sentence."
  (evil-select-inner-object 'evil-sentence beg end type count))

(evil-define-text-object evil-a-paragraph (count &optional beg end type)
  "Select a paragraph."
  :type line
  (evil-select-an-object 'evil-paragraph beg end type count t))

(evil-define-text-object evil-inner-paragraph (count &optional beg end type)
  "Select inner paragraph."
  :type line
  (evil-select-inner-object 'evil-paragraph beg end type count t))

(evil-define-text-object evil-a-paren (count &optional beg end type)
  "Select a parenthesis."
  :extend-selection nil
  (evil-select-paren ?\( ?\) beg end type count t))

(evil-define-text-object evil-inner-paren (count &optional beg end type)
  "Select inner parenthesis."
  :extend-selection nil
  (evil-select-paren ?\( ?\) beg end type count))

(evil-define-text-object evil-a-bracket (count &optional beg end type)
  "Select a square bracket."
  :extend-selection nil
  (evil-select-paren ?\[ ?\] beg end type count t))

(evil-define-text-object evil-inner-bracket (count &optional beg end type)
  "Select inner square bracket."
  :extend-selection nil
  (evil-select-paren ?\[ ?\] beg end type count))

(evil-define-text-object evil-a-curly (count &optional beg end type)
  "Select a curly bracket (\"brace\")."
  :extend-selection nil
  (evil-select-paren ?{ ?} beg end type count t))

(evil-define-text-object evil-inner-curly (count &optional beg end type)
  "Select inner curly bracket (\"brace\")."
  :extend-selection nil
  (evil-select-paren ?{ ?} beg end type count))

(evil-define-text-object evil-an-angle (count &optional beg end type)
  "Select an angle bracket."
  :extend-selection nil
  (evil-select-paren ?< ?> beg end type count t))

(evil-define-text-object evil-inner-angle (count &optional beg end type)
  "Select inner angle bracket."
  :extend-selection nil
  (evil-select-paren ?< ?> beg end type count))

(evil-define-text-object evil-a-single-quote (count &optional beg end type)
  "Select a single-quoted expression."
  :extend-selection t
  (evil-select-quote ?' beg end type count t))

(evil-define-text-object evil-inner-single-quote (count &optional beg end type)
  "Select inner single-quoted expression."
  :extend-selection nil
  (evil-select-quote ?' beg end type count))

(evil-define-text-object evil-a-double-quote (count &optional beg end type)
  "Select a double-quoted expression."
  :extend-selection t
  (evil-select-quote ?\" beg end type count t))

(evil-define-text-object evil-inner-double-quote (count &optional beg end type)
  "Select inner double-quoted expression."
  :extend-selection nil
  (evil-select-quote ?\" beg end type count))

(evil-define-text-object evil-a-back-quote (count &optional beg end type)
  "Select a back-quoted expression."
  :extend-selection t
  (evil-select-quote ?\` beg end type count t))

(evil-define-text-object evil-inner-back-quote (count &optional beg end type)
  "Select inner back-quoted expression."
  :extend-selection nil
  (evil-select-quote ?\` beg end type count))

(evil-define-text-object evil-a-tag (count &optional beg end type)
  "Select a tag block."
  :extend-selection nil
  (evil-select-xml-tag beg end type count t))

(evil-define-text-object evil-inner-tag (count &optional beg end type)
  "Select inner tag block."
  :extend-selection nil
  (evil-select-xml-tag beg end type count))

(defun evil-match (direction count)
  "Find COUNTth next match in DIRECTION."
  (unless (and (boundp 'evil-search-module)
               (eq evil-search-module 'evil-search))
    (user-error "Match text objects only work with Evil search module"))
  (let ((pnt (point))
        (count (abs count)) ;; Undo effect of evil-visual-direction
        (evil-ex-search-direction 'backward)
        (visual-state (evil-visual-state-p))
        on-start-match in-match on-end-match)
    (save-excursion
      (unless (eobp) (forward-char)) ;; If on start of a match, stay there
      (evil-ex-search 1)
      (setq on-start-match (= evil-ex-search-match-beg pnt)
            in-match (<= evil-ex-search-match-beg pnt (1- evil-ex-search-match-end))
            on-end-match (= (1- evil-ex-search-match-end) pnt)
            evil-ex-search-direction direction)
      (cond
       ((and visual-state on-start-match (eq 'backward direction))
        (evil-ex-search count))
       ((and visual-state on-end-match (eq 'forward direction))
        (evil-ex-search count))
       ((or in-match (eq 'backward direction))
        (evil-ex-search (1- count)))
       (t (evil-ex-search count)))
      (setq pnt (point)))
    (goto-char pnt)
    (cond
     ((evil-normal-state-p)
      (evil-visual-select evil-ex-search-match-beg
                          evil-ex-search-match-end
                          'inclusive
                          (pcase direction ('forward +1) ('backward -1))
                          t)
      (list evil-ex-search-match-beg evil-ex-search-match-end))
     ((and visual-state (eq 'forward direction))
      (goto-char (1- evil-ex-search-match-end)))
     ((and visual-state (eq 'backward direction))
      (goto-char evil-ex-search-match-beg))
     ;; e.g. operator pending...
     (t (list evil-ex-search-match-beg evil-ex-search-match-end)))))

(evil-define-text-object evil-next-match (count &optional beg end type)
  "Select next match."
  :extend-selection t
  (evil-match 'forward count))

(evil-define-text-object evil-previous-match (count &optional beg end type)
  "Select previous match."
  :extend-selection t
  (evil-match 'backward count))

;;; Operator commands

(evil-define-operator evil-yank (beg end type register yank-handler)
  "Save the characters in motion into the kill-ring."
  :move-point nil
  :repeat nil
  (interactive "<R><x><y>")
  (let ((evil-was-yanked-without-register
         (and evil-was-yanked-without-register (not register))))
    (cond
     ((and (fboundp 'cua--global-mark-active)
           (fboundp 'cua-copy-region-to-global-mark)
           (cua--global-mark-active))
      (cua-copy-region-to-global-mark beg end))
     ((eq type 'block)
      (evil-yank-rectangle beg end register yank-handler))
     ((memq type '(line screen-line))
      (evil-yank-lines beg end register yank-handler))
     (t
      (evil-yank-characters beg end register yank-handler)
      (goto-char beg)))))

(evil-define-operator evil-yank-line (beg end type register)
  "Save whole lines into the kill-ring."
  :motion evil-line-or-visual-line
  :move-point nil
  (interactive "<R><x>")
  (when (evil-visual-state-p)
    (unless (memq type '(line block screen-line))
      (let ((range (evil-expand beg end
                                (if (and evil-respect-visual-line-mode
                                         visual-line-mode)
                                    'screen-line
                                  'line))))
        (setq beg (evil-range-beginning range)
              end (evil-range-end range)
              type (evil-type range))))
    (evil-exit-visual-state))
  (evil-yank beg end type register))

(evil-define-operator evil-delete (beg end type register yank-handler)
  "Delete text from BEG to END with TYPE.
Save in REGISTER or in the kill-ring with YANK-HANDLER."
  (interactive "<R><x><y>")
  (when (and (memq type '(inclusive exclusive))
             (not (evil-visual-state-p))
             (eq 'evil-delete evil-this-operator)
             (save-excursion (goto-char beg) (bolp))
             (save-excursion (goto-char end) (eolp))
             (<= 1 (evil-count-lines beg end)))
    ;; Imitate Vi strangeness: if motion meets above criteria,
    ;; delete linewise. Not for change operator or visual state.
    (let ((new-range (evil-line-expand beg end)))
      (setq beg (car new-range)
            end (cadr new-range)
            type 'line)))
  (unless register
    (let ((text (filter-buffer-substring beg end)))
      (unless (string-match-p "\n" text)
        ;; set the small delete register
        (evil-set-register ?- text))))
  (let ((evil-was-yanked-without-register nil))
    (evil-yank beg end type register yank-handler))
  (cond
   ((eq type 'block)
    (evil-apply-on-block #'delete-region beg end nil))
   ((and (eq type 'line)
         (= end (point-max))
         (or (= beg end)
             (/= (char-before end) ?\n))
         (/= beg (point-min))
         (=  (char-before beg) ?\n))
    (delete-region (1- beg) end))
   (t (delete-region beg end)))
  (when (and (eq type 'line)
             (called-interactively-p 'any))
    (evil-first-non-blank)
    (when (and (not evil-start-of-line)
               evil-operator-start-col
               ;; Special exceptions to ever saving column:
               (not (memq evil-this-motion '(evil-forward-word-begin
                                             evil-forward-WORD-begin))))
      (move-to-column evil-operator-start-col))))

(evil-define-operator evil-delete-line (beg end type register yank-handler)
  "Delete to end of line."
  :motion evil-end-of-line-or-visual-line
  (interactive "<R><x>")
  ;; Act linewise in Visual state
  (when (and (evil-visual-state-p) (eq type 'inclusive))
    (let ((range (evil-expand
                  beg end
                  (if (and evil-respect-visual-line-mode visual-line-mode)
                      'screen-line 'line))))
      (setq beg (car range)
            end (cadr range)
            type (evil-type range))))
  (if (eq type 'block)
      ;; Equivalent to $d, i.e., we use the block-to-eol selection and
      ;; call `evil-delete'. In this case we fake the call to
      ;; `evil-end-of-line' by setting `temporary-goal-column' and
      ;; `last-command' appropriately as `evil-end-of-line' would do.
      (let ((temporary-goal-column most-positive-fixnum)
            (last-command 'next-line))
        (evil-delete beg end 'block register yank-handler))
    (evil-delete beg end type register yank-handler)))

(evil-define-operator evil-delete-whole-line
  (beg end type register yank-handler)
  "Delete whole line."
  :motion evil-line-or-visual-line
  (interactive "<R><x>")
  (evil-delete beg end type register yank-handler))

(evil-define-operator evil-delete-char (beg end type register)
  "Delete next character."
  :motion evil-forward-char
  (interactive "<R><x>")
  (evil-delete beg end type register))

(evil-define-operator evil-delete-backward-char (beg end type register)
  "Delete previous character."
  :motion evil-backward-char
  (interactive "<R><x>")
  (evil-delete beg end type register))

(evil-define-command evil-delete-backward-char-and-join (count)
  "Delete previous character and join lines.
If point is at the beginning of a line then the current line will
be joined with the previous line if and only if
`evil-backspace-join-lines'."
  (interactive "p")
  (if (or evil-backspace-join-lines (not (bolp)))
      (call-interactively 'delete-backward-char)
    (user-error "Beginning of line")))

(evil-define-command evil-delete-backward-word ()
  "Delete previous word."
  (let ((beg (save-excursion (evil-backward-word-begin) (point)))
        (end (point)))
    (cond
     ((evil-replace-state-p) (while (< beg (point))
                               (evil-replace-backspace)))
     ((or (not (bolp)) (bobp)) (delete-region (max beg (line-beginning-position))
                                              end))
     (evil-backspace-join-lines (delete-char -1))
     (t (user-error "Beginning of line")))))

(evil-define-command evil-delete-back-to-indentation ()
  "Delete back to the first non-whitespace character.
If point is before the first non-whitespace character of a
current line then delete from the point to the beginning of the
current line.  If point is on the beginning of the line, behave
according to `evil-backspace-join-lines'."
  (let ((beg (if (<= (current-column) (current-indentation))
                 (line-beginning-position)
               (save-excursion
                 (evil-first-non-blank)
                 (point)))))
    (cond
     ((and (bolp) (evil-replace-state-p)) (evil-replace-backspace))
     ((bolp) (evil-delete-backward-char-and-join 1))
     ((evil-replace-state-p) (while (< beg (point))
                               (evil-replace-backspace)))
     (t (delete-region beg (point))))))

(defun evil-ex-delete-or-yank (should-delete beg end type register count yank-handler)
  "Execute evil-delete or evil-yank on the given region.
If SHOULD-DELETE is t, evil-delete will be executed, otherwise
evil-yank.
The region specified by BEG and END will be adjusted if COUNT is
given."
  (when count
    ;; with COUNT, the command should go the end of the region and delete/yank
    ;; COUNT lines from there
    (setq beg (save-excursion
                (goto-char end)
                (forward-line -1)
                (point))
          end (save-excursion
                (goto-char end)
                (line-beginning-position count))
          type 'line))
  (funcall (if should-delete 'evil-delete 'evil-yank) beg end type register yank-handler))

(evil-define-operator evil-ex-delete (beg end type register count yank-handler)
  "The Ex delete command.
\[BEG,END]delete [REGISTER] [COUNT]"
  (interactive "<R><xc/><y>")
  (evil-ex-delete-or-yank t beg end type register count yank-handler))

(evil-define-operator evil-ex-yank (beg end type register count yank-handler)
  "The Ex yank command.
\[BEG,END]yank [REGISTER] [COUNT]"
  :restore-point t
  (interactive "<R><xc/><y>")
  (evil-ex-delete-or-yank nil beg end type register count yank-handler))

(evil-define-command evil-ex-put (beg end ex-arg &optional force)
  (interactive "<r><a><!>")
  (let* ((arg-chars (remove ?\s (string-to-list ex-arg)))
         (reg (or (car arg-chars) ?\"))
         (text (cond
                ((and (< 1 (length arg-chars))
                      (/= ?= reg))
                 (user-error "Trailing characters"))
                ((eq ?= reg)
                 (evil--eval-expr (if (= 1 (length arg-chars))
                                      evil-last-=-register-input
                                    (setq evil-last-=-register-input (substring ex-arg 1)))))
                (t (evil-get-register reg)))))
    (unless text (user-error "Nothing in register %c" reg))
    (evil-remove-yank-excluded-properties text)
    (goto-char (if (= (point-max) end) end (1- end)))
    (if force (evil-insert-newline-above) (evil-insert-newline-below))
    (evil-set-marker ?\[ (point))
    ;; `insert' rather than `insert-for-yank' as we want to ignore yank-handlers...
    (insert (if (and (< 0 (length text))
                     (eq ?\n (aref text (1- (length text)))))
                (substring text 0 (1- (length text)))
              text))
    (evil-set-marker ?\] (1- (point)))
    (back-to-indentation)
    (evil-normal-state)))

(evil-define-operator evil-change
  (beg end type register yank-handler delete-func)
  "Change text from BEG to END with TYPE.
Save in REGISTER or the kill-ring with YANK-HANDLER.
DELETE-FUNC is a function for deleting text, default `evil-delete'.
If TYPE is `line', insertion starts on an empty line.
If TYPE is `block', the inserted text in inserted at each line
of the block."
  (interactive "<R><x><y>")
  (let ((delete-func (or delete-func #'evil-delete))
        (nlines (1+ (evil-count-lines beg end)))
        opoint leftmost-point)
    (save-excursion
      (goto-char beg)
      (setq opoint (line-beginning-position))
      (setq leftmost-point
            (let ((inhibit-field-text-motion t)) (line-beginning-position))))
    (unless (eq evil-want-fine-undo t)
      (evil-start-undo-step))
    (funcall delete-func beg end type register yank-handler)
    (cond
     ((eq type 'line)
      (setq this-command 'evil-change-whole-line) ; for evil-maybe-remove-spaces
      (cond
       ((/= opoint leftmost-point) (evil-insert 1)) ; deletion didn't delete line
       ((= opoint (point)) (evil-open-above 1))
       (t (evil-open-below 1))))
     ((eq type 'block)
      (evil-insert 1 nlines))
     (t
      (evil-insert 1)))
    (setq evil-this-register nil)))

(evil-define-operator evil-change-line (beg end type register yank-handler)
  "Change to end of line, or change whole line if characterwise visual mode."
  :motion evil-end-of-line-or-visual-line
  (interactive "<R><x><y>")
  (if (and (evil-visual-state-p) (eq type 'inclusive))
      (cl-destructuring-bind (beg end &rest) (evil-line-expand beg end)
        (evil-change-whole-line beg end register yank-handler))
    (evil-change beg end type register yank-handler #'evil-delete-line)))

(evil-define-operator evil-change-whole-line
  (beg end register yank-handler)
  "Change whole line."
  :motion evil-line-or-visual-line
  :type line
  (interactive "<r><x>")
  (evil-change beg end 'line register yank-handler))

(evil-define-command evil-copy (beg end address)
  "Copy lines in BEG END below line given by ADDRESS."
  :motion evil-line-or-visual-line
  (interactive "<r><addr>")
  (goto-char (point-min))
  (forward-line address)
  (let* ((txt (buffer-substring-no-properties beg end))
         (len (length txt)))
    ;; ensure text consists of complete lines
    (when (or (zerop len) (/= (aref txt (1- len)) ?\n))
      (setq txt (concat txt "\n")))
    (when (and (eobp) (not (bolp))) (newline)) ; incomplete last line
    (insert txt)
    (forward-line -1)))

(evil-define-command evil-move (beg end address)
  "Move lines in BEG END below line given by ADDRESS."
  :motion evil-line-or-visual-line
  (interactive "<r><addr>")
  (unless (= (1+ address) (line-number-at-pos beg))
    (goto-char (point-min))
    (forward-line address)
    (let* ((m (set-marker (make-marker) (point)))
           (txt (buffer-substring-no-properties beg end))
           (len (length txt))
           (last-line-blank (progn (goto-char (point-max)) (bolp))))
      (delete-region beg end)
      (unless last-line-blank ; as vim, preserve lack of blank last line
        (progn (goto-char (point-max)) (when (bolp) (delete-char -1))))
      (goto-char m)
      (set-marker m nil)
      ;; ensure text consists of complete lines
      (when (or (zerop len) (/= (aref txt (1- len)) ?\n))
        (setq txt (concat txt "\n")))
      (when (and (eobp) (not (bolp))) (newline)) ; incomplete last line
      (when (evil-visual-state-p)
        (move-marker evil-visual-mark (point)))
      (insert txt)
      (forward-line -1)
      (when (evil-visual-state-p)
        (move-marker evil-visual-point (point))))))

(defun evil--check-undo-system ()
  (when (and (eq evil-undo-system 'undo-tree)
             (not (bound-and-true-p undo-tree-mode)))
    (user-error "Enable `global-undo-tree-mode' to use undo-tree commands.
Add (add-hook 'evil-local-mode-hook 'turn-on-undo-tree-mode) to your init file for undo in non-file buffers.")))

(evil-define-command evil-undo (count)
  "Undo COUNT changes in buffer using `evil-undo-function'."
  :repeat abort
  (interactive "*p")
  (evil--check-undo-system)
  (funcall evil-undo-function count))

(evil-define-command evil-redo (count)
  "Undo COUNT changes in buffer using `evil-redo-function'."
  :repeat abort
  (interactive "*p")
  (evil--check-undo-system)
  (funcall evil-redo-function count))

(evil-define-operator evil-substitute (beg end type register)
  "Change a character."
  :motion evil-forward-char
  (interactive "<R><x>")
  (evil-change beg end type register))

(evil-define-operator evil-upcase (beg end type)
  "Convert text to upper case."
  (if (eq type 'block)
      (evil-apply-on-block #'evil-upcase beg end nil)
    (upcase-region beg end)))

(evil-define-operator evil-downcase (beg end type)
  "Convert text to lower case."
  (if (eq type 'block)
      (evil-apply-on-block #'evil-downcase beg end nil)
    (downcase-region beg end)))

(evil-define-operator evil-invert-case (beg end type)
  "Invert case of text."
  (let (char)
    (if (eq type 'block)
        (evil-apply-on-block #'evil-invert-case beg end nil)
      (save-excursion
        (goto-char beg)
        (while (< beg end)
          (setq char (following-char))
          (delete-char 1 nil)
          (if (eq (upcase char) char)
              (insert-char (downcase char) 1)
            (insert-char (upcase char) 1))
          (setq beg (1+ beg)))))))

(evil-define-operator evil-invert-char (beg end type)
  "Invert case of character."
  :motion evil-forward-char
  (if (eq type 'block)
      (evil-apply-on-block #'evil-invert-case beg end nil)
    (evil-invert-case beg end)
    (when evil-this-motion
      (goto-char end)
      (when (and evil-cross-lines
                 (not evil-move-beyond-eol)
                 (not (evil-visual-state-p))
                 (not (evil-operator-state-p))
                 (eolp) (not (eobp)) (not (bolp)))
        (forward-char)))))

(evil-define-operator evil-rot13 (beg end type)
  "ROT13 encrypt text."
  (if (eq type 'block)
      (evil-apply-on-block #'evil-rot13 beg end nil)
    (rot13-region beg end)))

(evil-define-operator evil-join (beg end)
  "Join the selected lines."
  :motion evil-line
  (let ((count (count-lines beg end)))
    (when (> count 1)
      (setq count (1- count)))
    (goto-char beg)
    (dotimes (_ count)
      (join-line 1))))

(evil-define-operator evil-join-whitespace (beg end)
  "Join the selected lines without changing whitespace.
\\<evil-normal-state-map>Like \\[evil-join], \
but doesn't insert or remove any spaces."
  :motion evil-line
  (let ((count (count-lines beg end)))
    (when (> count 1)
      (setq count (1- count)))
    (goto-char beg)
    (dotimes (_ count)
      (evil-move-end-of-line 1)
      (unless (eobp)
        (delete-char 1)))))

(evil-define-operator evil-ex-join (beg end &optional count bang)
  "Join the selected lines with optional COUNT and BANG."
  (interactive "<r><a><!>")
  (if (and count (not (string-match-p "^[1-9][0-9]*$" count)))
      (user-error "Invalid count")
    (let ((join-fn (if bang 'evil-join-whitespace 'evil-join)))
      (cond
       ((not count)
        ;; without count - just join the given region
        (funcall join-fn beg end))
       (t
        ;; emulate vim's :join when count is given - start from the
        ;; end of the region and join COUNT lines from there
        (let* ((count-num (string-to-number count))
               (beg-adjusted (save-excursion
                               (goto-char end)
                               (forward-line -1)
                               (point)))
               (end-adjusted (save-excursion
                               (goto-char end)
                               (line-beginning-position count-num))))
          (funcall join-fn beg-adjusted end-adjusted)))))))

(evil-define-operator evil-fill (beg end)
  "Fill text."
  :move-point nil
  :type line
  (save-excursion
    (condition-case nil
        (fill-region beg end)
      (error nil))))

(evil-define-operator evil-fill-and-move (beg end)
  "Fill text and move point to the end of the filled region."
  :move-point nil
  :type line
  (let ((marker (make-marker)))
    (move-marker marker (1- end))
    (condition-case nil
        (progn
          (fill-region beg end)
          (goto-char marker)
          (evil-first-non-blank))
      (error nil))))

(evil-define-operator evil-indent (beg end)
  "Indent text."
  :move-point nil
  :type line
  (evil-ensure-column
    ;; Reset unneeded change made by evil-ensure-column
    (setq this-command real-this-command)
    (save-restriction
      (narrow-to-region beg end)
      (if (and (= beg (line-beginning-position))
               (= end (line-beginning-position 2)))
          ;; since some Emacs modes can only indent one line at a time,
          ;; implement "==" as a call to `indent-according-to-mode'
          (indent-according-to-mode)
        (goto-char beg)
        (indent-region beg end))
      ;; Update `beg' and `end'
      (setq beg (point-min)
            end (point-max))
      ;; We also need to tabify or untabify the leading white characters
      (when evil-indent-convert-tabs
        (let* ((beg-line (line-number-at-pos beg))
               (end-line (line-number-at-pos end))
               (ln beg-line)
               (convert-white (if indent-tabs-mode 'tabify 'untabify)))
          (save-excursion
            (while (<= ln end-line)
              (goto-char (point-min))
              (forward-line (- ln 1))
              (back-to-indentation)
              ;; Whether tab or space should be used is determined by indent-tabs-mode
              (funcall convert-white (line-beginning-position) (point))
              (setq ln (1+ ln)))))))))

(evil-define-operator evil-indent-line (beg end)
  "Indent the line."
  :motion evil-line
  (evil-indent beg end))

(evil-define-operator evil-shift-left (beg end &optional count preserve-empty)
  "Shift text from BEG to END to the left.
The text is shifted to the nearest multiple of `evil-shift-width'
\(the rounding can be disabled by setting `evil-shift-round').
If PRESERVE-EMPTY is non-nil, lines that contain only spaces are
indented, too, otherwise they are ignored.  Location of point
is preserved relative to text when called from insert state.
Otherwise, it is determined by `evil-start-of-line' and/or `evil-track-eol'.
See also `evil-shift-right'."
  :type line
  (interactive "<r><vc>")
  (evil-shift-right beg end (- (or count 1)) preserve-empty))

(evil-define-operator evil-shift-right (beg end &optional count preserve-empty)
  "Shift text from BEG to END to the right.
The text is shifted to the nearest multiple of `evil-shift-width'
\(the rounding can be disabled by setting `evil-shift-round').
If PRESERVE-EMPTY is non-nil, lines that contain only spaces are
indented, too, otherwise they are ignored.  Location of point
is preserved relative to text when called from insert or replace states.
Otherwise, it is determined by `evil-start-of-line' and/or `evil-track-eol'.
See also `evil-shift-left'."
  :type line
  :move-point nil ; point is moved according to `evil-start-of-line' and state
  (interactive "<r><vc>")
  (setq count (or count 1))
  (let ((beg (set-marker (make-marker) beg))
        (end (set-marker (make-marker) end))
        (col-for-insert (current-column))
        first-shift) ; shift of first line
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (let* ((indent (current-indentation))
               (new-indent
                (max 0
                     (if (not evil-shift-round)
                         (+ indent (* count evil-shift-width))
                       (* (+ (/ indent evil-shift-width)
                             count
                             (cond
                              ((> count 0) 0)
                              ((zerop (mod indent evil-shift-width)) 0)
                              (t 1)))
                          evil-shift-width)))))
          (unless first-shift
            (setq first-shift (- new-indent indent)))
          (when (or preserve-empty
                    (save-excursion
                      (skip-chars-forward " \t")
                      (not (eolp))))
            (indent-to new-indent 0))
          (delete-region (point) (progn (skip-chars-forward " \t") (point)))
          (forward-line 1))))
    ;; in case we're in an empty buffer first-shift is still unchanged
    (unless first-shift
      (if (< count 0)
          (setq first-shift 0)
        (setq first-shift (* count evil-shift-width))
        (indent-to first-shift)))
    ;; When called from insert state (C-t or C-d) the cursor should shift with the line,
    ;; otherwise (normal state) its position is determined by `evil-start-of-line'.
    (cond
     ((or (evil-insert-state-p) (evil-replace-state-p))
      (move-to-column (max 0 (+ col-for-insert first-shift))))
     (evil-start-of-line (evil-first-non-blank))
     ((evil--stick-to-eol-p) (move-end-of-line 1))
     (t (move-to-column (or goal-column evil-operator-start-col col-for-insert))))))

(defun evil-delete-indentation ()
  "Delete all indentation on current line."
  (interactive)
  (save-excursion
    (evil-beginning-of-line)
    (delete-region (point) (progn (skip-chars-forward " \t") (point)))))

(evil-define-command evil-shift-right-line (count)
  "Shift the current line COUNT times to the right.
The text is shifted to the nearest multiple of
`evil-shift-width'. Like `evil-shift-right' but always works on
the current line."
  (interactive "<c>")
  (evil-shift-right (line-beginning-position) (line-beginning-position 2) count t))

(evil-define-command evil-shift-left-line (count)
  "Shift the current line COUNT times to the left.
The text is shifted to the nearest multiple of
`evil-shift-width'. Like `evil-shift-left' but always works on
the current line."
  (interactive "<c>")
  (if (and (eq 'self-insert-command last-command)
           (eq ?0 (char-before)))
      (progn (backward-delete-char 1)
             (evil-delete-indentation))
    (evil-shift-left (line-beginning-position) (line-beginning-position 2) count t)))

(evil-define-operator evil-align-left (beg end type &optional width)
  "Left-align lines in the region at WIDTH columns.
The default for width is the value of `fill-column'."
  :motion evil-line
  :type line
  (interactive "<R><a>")
  (evil-justify-lines beg end 'left (if width
                                        (string-to-number width)
                                      0)))

(evil-define-operator evil-align-right (beg end type &optional width)
  "Right-align lines in the region at WIDTH columns.
The default for width is the value of `fill-column'."
  :motion evil-line
  :type line
  (interactive "<R><a>")
  (evil-justify-lines beg end 'right (if width
                                         (string-to-number width)
                                       fill-column)))

(evil-define-operator evil-align-center (beg end type &optional width)
  "Center lines in the region between WIDTH columns.
The default for width is the value of `fill-column'."
  :motion evil-line
  :type line
  (interactive "<R><a>")
  (evil-justify-lines beg end 'center (if width
                                          (string-to-number width)
                                        fill-column)))

(evil-define-operator evil-replace (beg end type char)
  "Replace text from BEG to END with CHAR."
  :motion evil-forward-char
  (interactive "<R>"
               (unwind-protect
                   (let ((evil-force-cursor 'replace))
                     (evil-refresh-cursor)
                     (list (evil-read-key)))
                 (evil-refresh-cursor)))
  (when char
    (if (eq type 'block)
        (save-excursion
          (evil-apply-on-rectangle
           #'(lambda (begcol endcol char)
               (let ((maxcol (evil-column (line-end-position))))
                 (when (< begcol maxcol)
                   (setq endcol (min endcol maxcol))
                   (let ((beg (evil-move-to-column begcol nil t))
                         (end (evil-move-to-column endcol nil t)))
                     (delete-region beg end)
                     (insert (make-string (- endcol begcol) char))))))
           beg end char))
      (goto-char beg)
      (cond
       ((eq char ?\n)
        (delete-region beg end)
        (newline)
        (when evil-auto-indent
          (indent-according-to-mode)))
       (t
        (while (< (point) end)
          (if (eq (char-after) ?\n)
              (forward-char)
            (delete-char 1)
            (insert-char char 1)))
        (goto-char (max beg (1- end))))))))

(evil-define-command evil-paste-before
  (count &optional register yank-handler)
  "Paste the latest yanked text before the cursor position.
The return value is the yanked text."
  :suppress-operator t
  (interactive "*P<x>")
  (setq count (prefix-numeric-value count))
  (if (evil-visual-state-p)
      ;; This is the only difference with evil-paste-after in visual-state
      (let ((evil-kill-on-visual-paste (not evil-kill-on-visual-paste)))
        (evil-visual-paste count register))
    (evil-with-undo
      (let* ((text (if register
                       (evil-get-register register)
                     (current-kill 0)))
             (yank-handler (or yank-handler
                               (when (stringp text)
                                 (car-safe (get-text-property
                                            0 'yank-handler text)))))
             (opoint (point)))
        (when evil-paste-clear-minibuffer-first
          (delete-minibuffer-contents)
          (setq evil-paste-clear-minibuffer-first nil))
        (when text
          (if (functionp yank-handler)
              (let ((evil-paste-count count)
                    ;; for non-interactive use
                    (this-command #'evil-paste-before))
                (push-mark opoint t)
                (insert-for-yank text))
            ;; no yank-handler, default
            (when (vectorp text)
              (setq text (evil-vector-to-string text)))
            (set-text-properties 0 (length text) nil text)
            (push-mark opoint t)
            (dotimes (_ (or count 1))
              (insert-for-yank text))
            (setq evil-last-paste
                  (list #'evil-paste-before
                        count
                        opoint
                        opoint    ; beg
                        (point))) ; end
            (evil-set-marker ?\[ opoint)
            (evil-set-marker ?\] (1- (point)))
            (when (and evil-move-cursor-back
                       (> (length text) 0))
              (backward-char))))
        (when evil--cursor-after
          (if (eq 'evil-yank-line-handler yank-handler)
              (ignore-errors (evil-next-line-first-non-blank 1))
            (evil-forward-char 1 nil t))
          (setq evil--cursor-after nil))
        ;; no paste-pop after pasting from a register
        (when register
          (setq evil-last-paste nil))
        (and (> (length text) 0) text)))))

(evil-define-command evil-paste-after
  (count &optional register yank-handler)
  "Paste the latest yanked text behind point.
The return value is the yanked text."
  :suppress-operator t
  (interactive "*P<x>")
  (setq count (prefix-numeric-value count))
  (if (evil-visual-state-p)
      (evil-visual-paste count register)
    (evil-with-undo
      (let* ((text (if register
                       (evil-get-register register)
                     (current-kill 0)))
             (yank-handler (or yank-handler
                               (when (stringp text)
                                 (car-safe (get-text-property
                                            0 'yank-handler text)))))
             (opoint (point)))
        (when text
          (if (functionp yank-handler)
              (let ((evil-paste-count count)
                    ;; for non-interactive use
                    (this-command #'evil-paste-after))
                (insert-for-yank text))
            ;; no yank-handler, default
            (when (vectorp text)
              (setq text (evil-vector-to-string text)))
            (set-text-properties 0 (length text) nil text)
            (unless (eolp) (forward-char))
            (push-mark (point) t)
            ;; TODO: Perhaps it is better to collect a list of all
            ;; (point . mark) pairs to undo the yanking for COUNT > 1.
            ;; The reason is that this yanking could very well use
            ;; `yank-handler'.
            (let ((beg (point)))
              (dotimes (_ (or count 1))
                (insert-for-yank text))
              (setq evil-last-paste
                    (list #'evil-paste-after
                          count
                          opoint
                          beg       ; beg
                          (point))) ; end
              (evil-set-marker ?\[ beg)
              (evil-set-marker ?\] (1- (point)))
              (when (evil-normal-state-p)
                (evil-move-cursor-back)))))
        (when evil--cursor-after
          (if (eq 'evil-yank-line-handler yank-handler)
              (ignore-errors (evil-next-line-first-non-blank 1))
            (evil-forward-char 1 nil t))
          (setq evil--cursor-after nil))
        (when register
          (setq evil-last-paste nil))
        (and (> (length text) 0) text)))))

(evil-define-command evil-paste-before-cursor-after
  (count &optional register yank-handler)
  "The same as `evil-paste-before' but
leave the cursor just after the new text."
  :suppress-operator t
  (interactive "*P<x>")
  (setq count (prefix-numeric-value count)
        evil--cursor-after t)
  (evil-paste-before count register yank-handler))

(evil-define-command evil-paste-after-cursor-after
  (count &optional register yank-handler)
  "The same as `evil-paste-after' but
leave the cursor just after the new text."
  :suppress-operator t
  (interactive "*P<x>")
  (setq count (prefix-numeric-value count)
        evil--cursor-after t)
  (evil-paste-after count register yank-handler))

(defun evil-insert-for-yank-at-col (startcol _endcol string count)
  "Insert STRING at STARTCOL."
  (move-to-column startcol)
  (dotimes (_ (or count 1))
    (insert-for-yank string))
  (evil-set-marker ?\] (1- (point))))

(evil-define-command evil-visual-paste (count &optional register)
  "Paste over Visual selection."
  :suppress-operator t
  (interactive "*P<x>")
  (setq count (prefix-numeric-value count))
  ;; evil-visual-paste is typically called from evil-paste-before or
  ;; evil-paste-after, but we have to mark that the paste was from
  ;; visual state
  (setq this-command 'evil-visual-paste)
  (let* ((text (if register
                   (evil-get-register register)
                 (current-kill 0)))
         (yank-handler (car-safe (get-text-property
                                  0 'yank-handler text)))
         (dir (evil-visual-direction))
         beg end paste-eob)
    (evil-with-undo
      (let ((kill-ring-yank-pointer (when kill-ring (list (current-kill 0)))))
        (when (evil-visual-state-p)
          (setq beg evil-visual-beginning
                end evil-visual-end)
          (evil-visual-rotate 'upper-left)
          ;; if we replace the last buffer line that does not end in a
          ;; newline, we use `evil-paste-after' because `evil-delete'
          ;; will move point to the line above
          (when (and (= evil-visual-end (point-max))
                     (/= (char-before (point-max)) ?\n))
            (setq paste-eob t))
          (evil-delete beg end (evil-visual-type) (unless evil-kill-on-visual-paste ?_))
          (when (and (eq yank-handler #'evil-yank-line-handler)
                     (not (memq (evil-visual-type) '(line block)))
                     (not (= evil-visual-end (point-max))))
            (insert "\n"))
          (evil-normal-state)
          (when kill-ring (current-kill 1)))
        ;; Effectively memoize `evil-get-register' because it can be
        ;; side-effecting (e.g. for the `=' register)...
        (cl-letf (((symbol-function 'evil-get-register)
                   (lambda (&rest _) text)))
          (cond
           ((eq 'block (evil-visual-type))
            (when (eq yank-handler #'evil-yank-line-handler)
              (setq text (concat "\n" text)))
            (evil-set-marker ?\[ beg)
            (evil-apply-on-block #'evil-insert-for-yank-at-col beg end t text count))
           (paste-eob (evil-paste-after count register))
           (t (evil-paste-before count register)))))
      (when evil-kill-on-visual-paste
        (current-kill -1))
      ;; Ensure that gv can restore visually pasted area...
      (setq evil-visual-previous-mark evil-visual-mark
            evil-visual-mark (evil-get-marker (if (<= 0 dir) ?\[ ?\]) t)
            evil-visual-previous-point evil-visual-point
            evil-visual-point (evil-get-marker (if (<= 0 dir) ?\] ?\[) t))
      ;; mark the last paste as visual-paste
      (setq evil-last-paste
            (list (nth 0 evil-last-paste)
                  (nth 1 evil-last-paste)
                  (nth 2 evil-last-paste)
                  (nth 3 evil-last-paste)
                  (nth 4 evil-last-paste)
                  t)))))

(defun evil-paste-from-register (register)
  "Paste from REGISTER."
  (interactive
   (let* ((opoint (point))
          (overlay (make-overlay opoint (+ opoint (if (evil-replace-state-p) 1 0)))))
     (unwind-protect
         (progn
           (overlay-put overlay 'invisible t)
           (overlay-put overlay 'after-string (propertize "\""
                                                          'face 'minibuffer-prompt
                                                          'cursor 1))
           (list (or evil-this-register (read-char))))
       (delete-overlay overlay))))
  (let ((opoint (point))
        (evil-move-cursor-back nil)
        reg-length chars-to-delete)
    (evil-paste-before nil register t)
    (when (evil-replace-state-p)
      (setq reg-length (- (point) opoint)
            chars-to-delete (min (- (line-end-position) (point)) reg-length))
      ;; TODO: handle multi-line paste backspacing
      (evil-update-replace-alist (point) reg-length chars-to-delete chars-to-delete)
      (delete-char chars-to-delete))))

(defun evil-paste-last-insertion ()
  "Paste last insertion."
  (interactive)
  (evil-paste-from-register ?.))

(defun evil-paste-last-insertion-and-stop-insert ()
  "Paste last insertion and change to normal state."
  (interactive)
  (evil-paste-last-insertion)
  (evil-normal-state))

(evil-define-command evil-use-register (register)
  "Use REGISTER for the next command."
  :keep-visual t
  :repeat ignore
  :suppress-operator t
  (interactive "<C>")
  (setq evil-this-register register))

(defvar evil-macro-buffer nil
  "The buffer that has been active on macro recording.")

(defvar evil-execute-normal-keys nil
  "The keys used to invoke the current `evil-execute-in-normal-state'.
Can be used to detect if we are currently in that quasi-state.
With current bindings, it will be \\<evil-insert-state-map>\\[evil-execute-in-normal-state]")

(defun evil-end-and-return-macro ()
  "Like `kmacro-end-macro' but also return the macro.
Remove \\<evil-insert-state-map>\\[evil-execute-in-normal-state] from the end."
  ;; `end-kbd-macro' rather than `kmacro-end-macro' to allow clearing registers
  (end-kbd-macro nil #'kmacro-loop-setup-function)
  (let ((end-keys-seq (append evil-execute-normal-keys nil))
        (last-kbd-macro-seq (append last-kbd-macro nil)))
    (unless last-kbd-macro-seq
      (setq last-kbd-macro nil))
    (if (and end-keys-seq last-kbd-macro-seq)
        (apply #'vector (butlast last-kbd-macro-seq (length end-keys-seq)))
      last-kbd-macro)))

(evil-define-command evil-record-macro (register)
  "Record a keyboard macro into REGISTER.
If REGISTER is :, /, or ?, the corresponding command line window
will be opened instead."
  :keep-visual t
  :suppress-operator t
  (interactive
   (list (unless (and evil-this-macro defining-kbd-macro)
           (or evil-this-register (evil-read-key)))))
  (let (last-macro)
    (cond
     ((eq register ?\C-g)
      (keyboard-quit))
     ((and evil-this-macro defining-kbd-macro)
      (setq evil-macro-buffer nil)
      (condition-case nil
          (setq last-macro (evil-end-and-return-macro))
        (error nil))
      (when last-macro
        (evil-set-register evil-this-macro last-macro))
      (setq evil-this-macro nil))
     ((eq register ?:)
      (evil-command-window-ex))
     ((eq register ?/)
      (evil-command-window-search-forward))
     ((eq register ??)
      (evil-command-window-search-backward))
     ((or (<= ?0 register ?9)
          (<= ?a register ?z)
          (<= ?A register ?Z))
      (when defining-kbd-macro (end-kbd-macro))
      (setq evil-this-macro register
            evil-last-recorded-register register)
      (evil-set-register evil-this-macro nil)
      (kmacro-start-macro nil)
      (setq evil-macro-buffer (current-buffer)))
     (t (error "Invalid register")))))

(evil-define-command evil-execute-macro (count macro)
  "Execute keyboard macro MACRO, COUNT times.
When called with a non-numerical prefix \
\(such as \\[universal-argument]),
COUNT is infinite. MACRO is read from a register
when called interactively."
  :keep-visual t
  :suppress-operator t
  (interactive
   (let (count macro register)
     (setq count (if current-prefix-arg
                     (if (numberp current-prefix-arg)
                         current-prefix-arg
                       0) 1)
           register (or evil-this-register (read-char)))
     (cond
      ((or (and (eq register ?@) (eq evil-last-register ?:))
           (eq register ?:))
       (setq macro (lambda () (evil-ex-repeat nil))
             evil-last-register ?:))
      ((eq register ?@)
       (unless evil-last-register
         (user-error "No previously executed keyboard macro."))
       (setq macro (evil-get-register evil-last-register t)))
      (t
       (setq macro (evil-get-register register t)
             evil-last-register register)))
     (list count macro)))
  (cond
   ((functionp macro)
    (evil-repeat-abort)
    (dotimes (_ (or count 1))
      (funcall macro)))
   ((or (and (not (stringp macro))
             (not (vectorp macro)))
        (member macro '("" [])))
    ;; allow references to currently empty registers
    ;; when defining macro
    (unless evil-this-macro
      (user-error "No previous macro")))
   (t
    (condition-case err
        (evil-with-single-undo
          (dotimes (_ (or count 1))
            (execute-kbd-macro macro)))
      ;; enter Normal state if the macro fails
      (error
       (evil-normal-state)
       (evil-normalize-keymaps)
       (signal (car err) (cdr err)))))))

(evil-define-command evil-execute-last-recorded-macro (count)
  "Execute last recorded keyboard macro COUNT times.
When called with a non-numerical prefix \
\(such as \\[universal-argument]),
COUNT is infinite."
  :keep-visual t
  :suppress-operator t
  (interactive
   (list (cond
          ((and current-prefix-arg (numberp current-prefix-arg)) current-prefix-arg)
          (current-prefix-arg 0)
          (t 1))))
  (if evil-last-recorded-register
      (evil-execute-macro count (evil-get-register evil-last-recorded-register t))
    (user-error "No previous macro"))
  (setq evil-last-register evil-last-recorded-register))

;;; Visual commands

(evil-define-motion evil-visual-restore ()
  "Restore previous selection."
  (let* ((point (point))
         (mark (or (mark t) point))
         (type (evil-visual-type)))
    ;; TODO handle swapping selection in visual state...
    (unless (evil-visual-state-p)
      (cond
       ;; No previous selection.
       ((or (null evil-visual-selection)
            (null evil-visual-mark)
            (null evil-visual-point)))
       (t
        (setq mark evil-visual-mark
              point evil-visual-point)))
      (evil-visual-make-selection mark point type t))))

(evil-define-motion evil-visual-exchange-corners ()
  "Rearrange corners in Visual Block mode.

        M---+           +---M
        |   |    <=>    |   |
        +---P           P---+

For example, if mark is in the upper left corner and point
in the lower right, this function puts mark in the upper right
corner and point in the lower left."
  (cond
   ((eq evil-visual-selection 'block)
    (let* ((point (point))
           (mark (or (mark t) point))
           (point-col (evil-column point))
           (mark-col (evil-column mark))
           (mark (save-excursion
                   (goto-char mark)
                   (evil-move-to-column point-col)
                   (point)))
           (point (save-excursion
                    (goto-char point)
                    (evil-move-to-column mark-col)
                    (point))))
      (evil-visual-refresh mark point)))
   (t
    (evil-exchange-point-and-mark)
    (evil-visual-refresh))))

(evil-define-command evil-visual-rotate (corner &optional beg end type)
  "In Visual Block selection, put point in CORNER.
Corner may be one of `upper-left', `upper-right', `lower-left'
and `lower-right':

        upper-left +---+ upper-right
                   |   |
        lower-left +---+ lower-right

When called interactively, the selection is rotated blockwise."
  :keep-visual t
  (interactive
   (let ((corners '(upper-left upper-right lower-right lower-left)))
     (list (or (cadr (memq (evil-visual-block-corner) corners))
               'upper-left))))
  (let* ((beg (or beg (point)))
         (end (or end (mark t) beg))
         (type (or type evil-this-type))
         range)
    (cond
     ((memq type '(rectangle block))
      (setq range (evil-block-rotate beg end :corner corner)
            beg (pop range)
            end (pop range))
      (unless (eq corner (evil-visual-block-corner corner beg end))
        (evil-swap beg end))
      (goto-char beg)
      (when (evil-visual-state-p)
        (evil-move-mark end)
        (evil-visual-refresh nil nil nil :corner corner)))
     ((memq corner '(upper-right lower-right))
      (goto-char (max beg end))
      (when (evil-visual-state-p)
        (evil-move-mark (min beg end))))
     (t
      (goto-char (min beg end))
      (when (evil-visual-state-p)
        (evil-move-mark (max beg end)))))))

;;; Insertion commands

(defun evil-insert (count &optional vcount skip-empty-lines)
  "Switch to Insert state just before point.
The insertion will be repeated COUNT times and repeated once for
the next VCOUNT - 1 lines starting at the same column.
If SKIP-EMPTY-LINES is non-nil, the insertion will not be performed
on lines on which the insertion point would be after the end of the
lines.  This is the default behaviour for Visual-state insertion."
  (interactive
   (list (prefix-numeric-value current-prefix-arg)
         (and (evil-visual-state-p)
              (memq (evil-visual-type) '(line block))
              (save-excursion
                (let ((m (mark)))
                  ;; go to upper-left corner temporarily so
                  ;; `count-lines' yields accurate results
                  (evil-visual-rotate 'upper-left)
                  (prog1 (count-lines evil-visual-beginning evil-visual-end)
                    (set-mark m)))))
         (evil-visual-state-p)))
  (if (and (called-interactively-p 'any)
           (evil-visual-state-p))
      (cond
       ((eq (evil-visual-type) 'line)
        (evil-visual-rotate 'upper-left)
        (evil-insert-line count vcount))
       ((eq (evil-visual-type) 'block)
        (let ((column (min (evil-column evil-visual-beginning)
                           (evil-column evil-visual-end))))
          (evil-visual-rotate 'upper-left)
          (move-to-column column t)
          (evil-insert count vcount skip-empty-lines)))
       (t
        (evil-visual-rotate 'upper-left)
        (evil-insert count vcount skip-empty-lines)))
    (setq evil-insert-count count
          evil-insert-lines nil
          evil-insert-vcount (and vcount
                                  (> vcount 1)
                                  (list (line-number-at-pos)
                                        (current-column)
                                        vcount))
          evil-insert-skip-empty-lines skip-empty-lines)
    (evil-insert-state 1)))

(defun evil-append (count &optional vcount skip-empty-lines)
  "Switch to Insert state just after point.
The insertion will be repeated COUNT times and repeated once for
the next VCOUNT - 1 lines starting at the same column.  If
SKIP-EMPTY-LINES is non-nil, the insertion will not be performed
on lines on which the insertion point would be after the end of
the lines."
  (interactive
   (list (prefix-numeric-value current-prefix-arg)
         (and (evil-visual-state-p)
              (memq (evil-visual-type) '(line block))
              (save-excursion
                (let ((m (mark)))
                  ;; go to upper-left corner temporarily so
                  ;; `count-lines' yields accurate results
                  (evil-visual-rotate 'upper-left)
                  (prog1 (count-lines evil-visual-beginning evil-visual-end)
                    (set-mark m)))))))
  (if (and (called-interactively-p 'any)
           (evil-visual-state-p))
      (cond
       ((or (eq (evil-visual-type) 'line)
            (and (eq (evil-visual-type) 'block)
                 (memq last-command '(next-line previous-line))
                 (eq temporary-goal-column most-positive-fixnum)))
        (evil-visual-rotate 'upper-left)
        (evil-append-line count vcount))
       ((eq (evil-visual-type) 'block)
        (let ((column (max (evil-column evil-visual-beginning)
                           (evil-column evil-visual-end))))
          (evil-visual-rotate 'upper-left)
          (move-to-column column t)
          (evil-insert count vcount skip-empty-lines)))
       (t
        (evil-visual-rotate 'lower-right)
        (backward-char)
        (evil-append count)))
    (unless (eolp) (forward-char))
    (evil-insert count vcount skip-empty-lines)
    (add-hook 'post-command-hook #'evil-maybe-remove-spaces)))

(defun evil-insert-resume (count)
  "Switch to Insert state at previous insertion point.
The insertion will be repeated COUNT times. If called from visual
state, only place point at the previous insertion position but do not
switch to insert state."
  (interactive "p")
  (evil-goto-mark ?^ t)
  (unless (evil-visual-state-p)
    (evil-insert count)))

(defun evil-quoted-insert (count)
  "Like `quoted-insert' but delete COUNT chars forward in replace state.
Adds a `^' overlay as an input prompt."
  (interactive "p")
  (let* ((opoint (point))
         chars-to-delete insert-prompt)
    (unwind-protect
        (progn
          (if (evil-replace-state-p)
              (progn
                (setq chars-to-delete (min (- (line-end-position) opoint) count)
                      insert-prompt (make-overlay opoint (+ chars-to-delete opoint)))
                (evil-update-replace-alist opoint count chars-to-delete))
            (setq insert-prompt (make-overlay opoint opoint)))
          (overlay-put insert-prompt 'invisible t)
          (overlay-put insert-prompt 'after-string (propertize "^"
                                                               'face 'escape-glyph
                                                               'cursor 1))
          (let (overwrite-mode) ;; Force `read-quoted-char'
            (quoted-insert count))
          (when (evil-replace-state-p) (delete-char chars-to-delete)))
      (when insert-prompt (delete-overlay insert-prompt)))))

(evil-define-command evil-open-above (count)
  "Insert a new line above point and switch to Insert state.
The insertion will be repeated COUNT times."
  :suppress-operator t
  (interactive "p")
  (unless (eq evil-want-fine-undo t)
    (evil-start-undo-step))
  (evil-insert-newline-above)
  (setq evil-insert-count count
        evil-insert-lines t
        evil-insert-vcount nil)
  (evil-insert-state 1)
  (when evil-auto-indent
    (indent-according-to-mode)))

(evil-define-command evil-open-below (count)
  "Insert a new line below point and switch to Insert state.
The insertion will be repeated COUNT times."
  :suppress-operator t
  (interactive "p")
  (unless (eq evil-want-fine-undo t)
    (evil-start-undo-step))
  (push (point) buffer-undo-list)
  (evil-insert-newline-below)
  (setq evil-insert-count count
        evil-insert-lines t
        evil-insert-vcount nil)
  (evil-insert-state 1)
  (when evil-auto-indent
    (indent-according-to-mode)))

(defun evil--insert-line (count vcount non-blank-p)
  "Switch to insert state at the beginning of the current line.
If NON-BLANK-P is non-nil, point is placed at the first non-blank character
on the current line.  If NON-BLANK-P is nil, point is placed at column 0,
or the beginning of visual line.  The insertion will be repeated COUNT times.
If VCOUNT is non nil it should be number > 0. The insertion will be repeated
in the next VCOUNT - 1 lines below the current one."
  (push (point) buffer-undo-list)
  (let ((move-fn (if non-blank-p #'back-to-indentation #'evil-beginning-of-line)))
    (if (and visual-line-mode
             evil-respect-visual-line-mode)
        (goto-char
         (max (save-excursion
                (funcall move-fn)
                (point))
              (save-excursion
                (beginning-of-visual-line)
                (point))))
      (funcall move-fn)))
  (setq evil-insert-count count
        evil-insert-lines nil
        evil-insert-vcount
        (and vcount
             (> vcount 1)
             (list (line-number-at-pos)
                   (if non-blank-p #'evil-first-non-blank #'evil-beginning-of-line)
                   vcount)))
  (evil-insert-state 1))

(defun evil-insert-line (count &optional vcount)
  "Switch to insert state at beginning of current line.
Point is placed at the first non-blank character on the current
line.  The insertion will be repeated COUNT times.  If VCOUNT is
non nil it should be number > 0. The insertion will be repeated
in the next VCOUNT - 1 lines below the current one."
  (interactive "p")
  (evil--insert-line count vcount t))

(defun evil-insert-0-line (count &optional vcount)
  "Switch to insert state at beginning of current line.
Point is placed at column 0, or the beginning of the visual line.
The insertion will be repeated COUNT times.  If VCOUNT is
non nil it should be number > 0. The insertion will be repeated
in the next VCOUNT - 1 lines below the current one."
  (interactive "p")
  (evil--insert-line count vcount nil))

(defun evil-append-line (count &optional vcount)
  "Switch to Insert state at the end of the current line.
The insertion will be repeated COUNT times.  If VCOUNT is non nil
it should be number > 0. The insertion will be repeated in the
next VCOUNT - 1 lines below the current one."
  (interactive "p")
  (if (and visual-line-mode
           evil-respect-visual-line-mode)
      (evil-end-of-visual-line)
    (evil-move-end-of-line))
  (setq evil-insert-count count
        evil-insert-lines nil
        evil-insert-vcount
        (and vcount
             (> vcount 1)
             (list (line-number-at-pos)
                   #'end-of-line
                   vcount)))
  (evil-insert-state 1))

(evil-define-command evil-insert-digraph (count)
  "Insert COUNT digraphs."
  :repeat change
  (interactive "p")
  (let ((opoint (point))
        chars-to-delete insert-prompt)
    (if (evil-replace-state-p)
        (progn
          (setq chars-to-delete (min (- (line-end-position) opoint) count)
                insert-prompt (make-overlay opoint (+ chars-to-delete opoint)))
          (evil-update-replace-alist opoint count chars-to-delete))
      (setq insert-prompt (make-overlay opoint opoint)))
    (insert-char (evil-read-digraph-char-with-overlay insert-prompt) count)
    (when chars-to-delete (delete-char chars-to-delete))))

(evil-define-command evil-ex-show-digraphs ()
  "Show a list of all available digraphs."
  :repeat nil
  (let ((columns 3))
    (evil-with-view-list
      :name "evil-digraphs"
      :mode-name "Evil Digraphs"
      :format
      (cl-loop repeat columns
               vconcat [("Digraph" 8 nil)
                        ("Sequence" 16 nil)])
      :entries
      (let* ((digraphs (mapcar #'(lambda (digraph)
                                   (cons (cdr digraph)
                                         (car digraph)))
                               (append evil-digraphs-table
                                       evil-digraphs-table-user)))
             (entries (cl-loop for digraph in digraphs
                               collect `(,(concat (char-to-string (nth 1 digraph))
                                                  (char-to-string (nth 2 digraph)))
                                         ,(char-to-string (nth 0 digraph)))))
             (row)
             (rows)
             (clength (* columns 2)))
        (cl-loop for e in entries
                 do
                 (push (nth 0 e) row)
                 (push (nth 1 e) row)
                 (when (eq (length row) clength)
                   (push `(nil ,(apply #'vector row)) rows)
                   (setq row nil)))
        rows))))

(defun evil--self-insert-string (string)
  "Insert STRING as if typed interactively."
  (let ((chars (append string nil)))
    (dolist (char chars)
      (let ((last-command-event char))
        (self-insert-command 1)))))

(defun evil-copy-from-above (arg)
  "Copy characters from preceding non-blank line.
The copied text is inserted before point.
ARG is the number of lines to move backward.
See also \\<evil-insert-state-map>\\[evil-copy-from-below]."
  (interactive
   (cond
    ;; if a prefix argument was given, repeat it for subsequent calls
    ((and (null current-prefix-arg)
          (eq last-command #'evil-copy-from-above))
     (setq current-prefix-arg last-prefix-arg)
     (list (prefix-numeric-value current-prefix-arg)))
    (t
     (list (prefix-numeric-value current-prefix-arg)))))
  (evil--self-insert-string (evil-copy-chars-from-line arg -1)))

(defun evil-copy-from-below (arg)
  "Copy characters from following non-blank line.
The copied text is inserted before point.
ARG is the number of lines to move forward.
See also \\<evil-insert-state-map>\\[evil-copy-from-above]."
  (interactive
   (cond
    ((and (null current-prefix-arg)
          (eq last-command #'evil-copy-from-below))
     (setq current-prefix-arg last-prefix-arg)
     (list (prefix-numeric-value current-prefix-arg)))
    (t
     (list (prefix-numeric-value current-prefix-arg)))))
  (evil--self-insert-string (evil-copy-chars-from-line arg 1)))

;; adapted from `copy-from-above-command' in misc.el
(defun evil-copy-chars-from-line (n num &optional col)
  "Return N characters from line NUM, starting at column COL.
NUM is relative to the current line and can be negative.
COL defaults to the current column."
  (interactive "p")
  (let ((col (or col (current-column))) prefix)
    (save-excursion
      (forward-line num)
      (when (looking-at "[[:space:]]*$")
        (if (< num 0)
            (skip-chars-backward " \t\n")
          (skip-chars-forward " \t\n")))
      (evil-move-beginning-of-line)
      (move-to-column col)
      ;; if the column winds up in middle of a tab,
      ;; return the appropriate number of spaces
      (when (< col (current-column))
        (if (eq (preceding-char) ?\t)
            (let ((len (min n (- (current-column) col))))
              (setq prefix (make-string len ?\s)
                    n (- n len)))
          ;; if in middle of a control char, return the whole char
          (backward-char 1)))
      (concat prefix
              (buffer-substring (point)
                                (min (line-end-position)
                                     (+ n (point))))))))

(defun evil-enter-replace-state (count)
  "Switch to Replace state at point.
The insertion will be repeated COUNT times."
  (interactive "p")
  (setq evil-insert-count count)
  (setq evil-insert-count count
        evil-insert-lines nil
        evil-insert-vcount nil)
  (evil-replace-state 1))

;; completion
(evil-define-command evil-complete-next (&optional arg)
  "Complete to the nearest following word.
Search backward if a match isn't found.
Calls `evil-complete-next-func'."
  :repeat change
  (interactive "P")
  (if (minibufferp)
      (funcall evil-complete-next-minibuffer-func)
    (funcall evil-complete-next-func arg)))

(evil-define-command evil-complete-previous (&optional arg)
  "Complete to the nearest preceding word.
Search forward if a match isn't found.
Calls `evil-complete-previous-func'."
  :repeat change
  (interactive "P")
  (if (minibufferp)
      (funcall evil-complete-previous-minibuffer-func)
    (funcall evil-complete-previous-func arg)))

(evil-define-command evil-complete-next-line (&optional arg)
  "Complete a whole line.
Calls `evil-complete-next-line-func'."
  :repeat change
  (interactive "P")
  (if (minibufferp)
      (funcall evil-complete-next-minibuffer-func)
    (funcall evil-complete-next-line-func arg)))

(evil-define-command evil-complete-previous-line (&optional arg)
  "Complete a whole line.
Calls `evil-complete-previous-line-func'."
  :repeat change
  (interactive "P")
  (if (minibufferp)
      (funcall evil-complete-previous-minibuffer-func)
    (funcall evil-complete-previous-line-func arg)))

;;; Search

(defun evil-repeat-search (flag)
  "Called to record a search command.
FLAG is either 'pre or 'post if the function is called before resp.
after executing the command."
  (cond
   ((and (evil-operator-state-p) (eq flag 'pre))
    (evil-repeat-record (this-command-keys))
    (evil-clear-command-keys))
   ((and (evil-operator-state-p) (eq flag 'post))
    ;; The value of (this-command-keys) at this point should be the
    ;; key-sequence that called the last command that finished the
    ;; search, usually RET. Therefore this key-sequence will be
    ;; recorded in the post-command of the operator. Alternatively we
    ;; could do it here.
    (evil-repeat-record (if evil-regexp-search
                            (car-safe regexp-search-ring)
                          (car-safe search-ring))))
   (t (evil-repeat-motion flag))))

(evil-define-motion evil-search-forward ()
  (format "Search forward for user-entered text.
Searches for regular expression if `evil-regexp-search' is t.%s"
          (if (and (fboundp 'isearch-forward)
                   (documentation 'isearch-forward))
              (format "\n\nBelow is the documentation string \
for `isearch-forward',\nwhich lists available keys:\n\n%s"
                      (documentation 'isearch-forward)) ""))
  :jump t
  :type exclusive
  :repeat evil-repeat-search
  (evil-search-incrementally t evil-regexp-search))

(evil-define-motion evil-search-backward ()
  (format "Search backward for user-entered text.
Searches for regular expression if `evil-regexp-search' is t.%s"
          (if (and (fboundp 'isearch-forward)
                   (documentation 'isearch-forward))
              (format "\n\nBelow is the documentation string \
for `isearch-forward',\nwhich lists available keys:\n\n%s"
                      (documentation 'isearch-forward)) ""))
  :jump t
  :type exclusive
  :repeat evil-repeat-search
  (evil-search-incrementally nil evil-regexp-search))

(evil-define-motion evil-search-next (count)
  "Repeat the last search."
  :jump t
  :type exclusive
  (let ((orig (point))
        (search-string (if evil-regexp-search
                           (car-safe regexp-search-ring)
                         (car-safe search-ring))))
    (goto-char
     ;; Wrap in `save-excursion' so that multiple searches have no visual effect.
     (save-excursion
       (evil-search search-string isearch-forward evil-regexp-search)
       (when (and (> (point) orig)
                  (save-excursion
                    (evil-adjust-cursor)
                    (= (point) orig)))
         ;; Point won't move after first attempt and `evil-adjust-cursor' takes
         ;; effect, so start again.
         (evil-search search-string isearch-forward evil-regexp-search))
       (point)))
    (when (and count (> count 1))
      (dotimes (_ (1- count))
        (evil-search search-string isearch-forward evil-regexp-search)))))

(evil-define-motion evil-search-previous (count)
  "Repeat the last search in the opposite direction."
  :jump t
  :type exclusive
  (dotimes (_ (or count 1))
    (evil-search (if evil-regexp-search
                     (car-safe regexp-search-ring)
                   (car-safe search-ring))
                 (not isearch-forward) evil-regexp-search)))

(evil-define-motion evil-search-word-backward (count &optional symbol)
  "Search backward for symbol under point."
  :jump t
  :type exclusive
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     evil-symbol-word-search))
  (dotimes (_ (or count 1))
    (evil-search-word nil nil symbol)))

(evil-define-motion evil-search-word-forward (count &optional symbol)
  "Search forward for symbol under point."
  :jump t
  :type exclusive
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     evil-symbol-word-search))
  (dotimes (_ (or count 1))
    (evil-search-word t nil symbol)))

(evil-define-motion evil-search-unbounded-word-backward (count &optional symbol)
  "Search backward for symbol under point.
The search is unbounded, i.e., the pattern is not wrapped in
\\<...\\>."
  :jump t
  :type exclusive
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     evil-symbol-word-search))
  (dotimes (_ (or count 1))
    (evil-search-word nil t symbol)))

(evil-define-motion evil-search-unbounded-word-forward (count &optional symbol)
  "Search forward for symbol under point.
The search is unbounded, i.e., the pattern is not wrapped in
\\<...\\>."
  :jump t
  :type exclusive
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     evil-symbol-word-search))
  (dotimes (_ (or count 1))
    (evil-search-word t t symbol)))

(defun evil-goto-definition-imenu (string _position)
  "Find definition for STRING with imenu."
  (require 'imenu nil t)
  (let (ientry ipos)
    (when (fboundp 'imenu--make-index-alist)
      (ignore-errors (setq ientry (imenu--make-index-alist)))
      (setq ientry (imenu--in-alist string ientry))
      (setq ipos (cdr ientry))
      (when (and (markerp ipos)
                 (eq (marker-buffer ipos) (current-buffer)))
        (setq ipos (marker-position ipos))
        (when (numberp ipos)
          (evil-search (format "\\_<%s\\_>" (regexp-quote string)) t t ipos)
          t)))))

(defun evil-goto-definition-semantic (_string position)
  "Find definition for POSITION with semantic."
  (and (fboundp 'semantic-ia-fast-jump)
       (ignore-errors (semantic-ia-fast-jump position))))

(declare-function xref-backend-identifier-at-point "xref")

(defun evil-goto-definition-xref (_string position)
  "Find definition at POSITION with xref."
  (when (fboundp 'xref-find-definitions)
    (let ((identifier (save-excursion
                        (goto-char position)
                        (xref-backend-identifier-at-point (xref-find-backend)))))
      (condition-case ()
          (progn
            (xref-find-definitions identifier)
            t)
        (user-error nil)))))

(defun evil-goto-definition-search (string _position)
  "Find definition for STRING with evil-search."
  (evil-search (format "\\_<%s\\_>" (regexp-quote string)) t t (point-min))
  t)

(evil-define-motion evil-goto-definition ()
  "Go to definition or first occurrence of symbol under point.
See also `evil-goto-definition-functions'."
  :jump t
  :type exclusive
  (let* ((match (evil--find-thing t 'symbol))
         (string (car match))
         (position (cdr match)))
    (if (null string)
        (user-error "No symbol under cursor")
      (setq isearch-forward t)
      (run-hook-with-args-until-success 'evil-goto-definition-functions
                                        string position))))

;;; Folding
(defun evil-fold-action (list action)
  "Perform fold ACTION for each matching major or minor mode in LIST.

ACTION will be performed for the first matching handler in LIST.  For more
information on its features and format, see the documentation for
`evil-fold-list'.

If no matching ACTION is found in LIST, an error will signaled.

Handler errors will be demoted, so a problem in one handler will (hopefully)
not interfere with another."
  (if (null list)
      (user-error
       "Enable one of the following modes for folding to work: %s"
       (mapconcat 'symbol-name (mapcar 'caar evil-fold-list) ", "))
    (let* ((modes (caar list)))
      (if (evil--mode-p modes)
          (let* ((actions (cdar list))
                 (fn      (plist-get actions action)))
            (when fn
              (with-demoted-errors "Error: %S" (funcall fn))))
        (evil-fold-action (cdr list) action)))))

(defun evil--mode-p (modes)
  "Determine whether any symbol in MODES represents the current
buffer's major mode or any of its minors."
  (unless (eq modes '())
    (let ((mode (car modes)))
      (or (eq major-mode mode)
          (and (boundp mode) (symbol-value mode))
          (evil--mode-p (cdr modes))))))

(evil-define-command evil-toggle-fold ()
  "Open or close a fold under point.
See also `evil-open-fold' and `evil-close-fold'."
  (evil-fold-action evil-fold-list :toggle))

(evil-define-command evil-open-folds ()
  "Open all folds.
See also `evil-close-folds'."
  (evil-fold-action evil-fold-list :open-all))

(evil-define-command evil-close-folds ()
  "Close all folds.
See also `evil-open-folds'."
  (evil-fold-action evil-fold-list :close-all))

(evil-define-command evil-open-fold ()
  "Open fold at point.
See also `evil-close-fold'."
  (evil-fold-action evil-fold-list :open))

(evil-define-command evil-open-fold-rec ()
  "Open fold at point recursively.
See also `evil-open-fold' and `evil-close-fold'."
  (evil-fold-action evil-fold-list :open-rec))

(evil-define-command evil-close-fold ()
  "Close fold at point.
See also `evil-open-fold'."
  (evil-fold-action evil-fold-list :close))

;;; Ex

(evil-define-operator evil-write (beg end type file-or-append &optional bang)
  "Save the current buffer, from BEG to END, to FILE-OR-APPEND.
If FILE-OR-APPEND is of the form \">> FILE\", append to FILE
instead of overwriting.  The current buffer's filename is not
changed unless it has no associated file and no region is
specified.  If the file already exists and the BANG argument is
non-nil, it is overwritten without confirmation."
  :motion nil
  :move-point nil
  :type line
  :repeat nil
  (interactive "<R><fsh><!>")
  (let* ((append-and-filename (evil-extract-append file-or-append))
         (append (car append-and-filename))
         (filename (cdr append-and-filename))
         (bufname (buffer-file-name (buffer-base-buffer))))
    (when (zerop (length filename))
      (setq filename bufname))
    (cond
     ((zerop (length filename))
      (user-error "Please specify a file name for the buffer"))
     ;; execute command on region
     ((eq (aref filename 0) ?!)
      (shell-command-on-region beg end (substring filename 1)))
     ;; with region or append, always save to file without resetting
     ;; modified flag
     ((or append (and beg end))
      (write-region beg end filename append nil nil (not (or append bang))))
     ;; no current file
     ((null bufname)
      (write-file filename (not bang)))
     ;; save current buffer to its file
     ((string= filename bufname)
      (if (not bang) (save-buffer) (write-file filename)))
     ;; save to another file
     (t
      (write-region nil nil filename
                    nil (not bufname) nil
                    (not bang))))))

(evil-define-command evil-write-all (bang)
  "Save all buffers visiting a file.
If BANG is non nil then read-only buffers are saved, too,
otherwise they are skipped. "
  :repeat nil
  :move-point nil
  (interactive "<!>")
  (if bang
      (save-some-buffers t)
    ;; save only buffer that are not read-only and
    ;; that are visiting a file
    (save-some-buffers t
                       #'(lambda ()
                           (and (not buffer-read-only)
                                (buffer-file-name))))))

(evil-define-command evil-update ()
  "Same as `evil-write', but only write when the buffer has been modified."
  :motion nil
  :move-point nil
  :type line
  :repeat nil
  (when (buffer-modified-p)
    (call-interactively #'evil-write)))

(evil-define-command evil-save (filename &optional bang)
  "Save the current buffer to FILENAME.
Changes the file name of the current buffer to FILENAME.  If no
FILENAME is given, the current file name is used."
  :repeat nil
  :move-point nil
  (interactive "<f><!>")
  (when (zerop (length filename))
    (setq filename (buffer-file-name (buffer-base-buffer))))
  (write-file filename (not bang)))

(evil-define-command evil-edit (file &optional bang)
  "Open FILE.
If no FILE is specified, reload the current buffer from disk."
  :repeat nil
  (interactive "<f><!>")
  (if file
      (find-file file)
    (revert-buffer bang (or bang (not (buffer-modified-p))) t)))

(evil-define-command evil-read (count file)
  "Insert the contents of FILE below the current line or line COUNT."
  :repeat nil
  :move-point nil
  (interactive "P<fsh>")
  (when (and file (not (zerop (length file))))
    (when count (goto-char (point-min)))
    (when (or (not (zerop (forward-line (or count 1))))
              (not (bolp)))
      (insert "\n"))
    (cond
     ((/= (aref file 0) ?!)
      (when (member file '("#" "%"))
        (setq file (evil-ex-replace-special-filenames file)))
      (let ((result (insert-file-contents file)))
        (save-excursion
          (forward-char (cadr result))
          (unless (bolp) (insert "\n")))))
     (t
      (shell-command (evil-ex-replace-special-filenames (substring file 1)) t)
      (goto-char (mark))
      (unless (bolp) (insert "\n"))
      (forward-line -1)))))

(evil-define-command evil-show-files ()
  "Show the file-list.
The same as `buffer-menu', but shows only buffers visiting
files."
  :repeat nil
  (buffer-menu 1))

(evil-define-command evil-goto-error (count)
  "Go to error number COUNT.

If no COUNT supplied, move to the current error.

Acts like `first-error' other than when given no counts, goes
to the current error instead of the first, like in Vim's :cc
command."
  :repeat nil
  (interactive "<c>")
  (if count
      (first-error (if (eql 0 count) 1 count))
    (next-error 0)))

(evil-define-command evil-buffer (buffer)
  "Switch to another buffer."
  :repeat nil
  (interactive "<b>")
  (cond
   ;; no buffer given, switch to "other" buffer
   ((null buffer) (switch-to-buffer (other-buffer)))
   ;; we are given the name of an existing buffer
   ((get-buffer buffer) (switch-to-buffer buffer))
   ;; try to complete the buffer
   ((let ((all-buffers (internal-complete-buffer buffer nil t)))
      (when (= (length all-buffers) 1)
        (switch-to-buffer (car all-buffers)))))
   (t
    (when (y-or-n-p
           (format "No buffer with name \"%s\" exists. Create new buffer? "
                   buffer))
      (switch-to-buffer buffer)))))

(evil-define-command evil-next-buffer (&optional count)
  "Go to the `count'-th next buffer in the buffer list."
  :repeat nil
  (interactive "p")
  (dotimes (_ (or count 1))
    (next-buffer)))

(evil-define-command evil-prev-buffer (&optional count)
  "Go to the `count'-th prev buffer in the buffer list."
  :repeat nil
  (interactive "p")
  (dotimes (_ (or count 1))
    (previous-buffer)))

(evil-define-command evil-delete-buffer (buffer &optional bang)
  "Delete a buffer.
All windows currently showing this buffer will be closed except
for the last window in each frame."
  (interactive "<b><!>")
  (with-current-buffer (or buffer (current-buffer))
    (when bang
      (set-buffer-modified-p nil)
      (dolist (process (process-list))
        (when (eq (process-buffer process) (current-buffer))
          (set-process-query-on-exit-flag process nil))))
    ;; get all windows that show this buffer
    (let ((wins (get-buffer-window-list (current-buffer) nil t)))
      ;; if the buffer which was initiated by emacsclient,
      ;; call `server-edit' from server.el to avoid
      ;; "Buffer still has clients" message
      (if (and (fboundp 'server-edit)
               (boundp 'server-buffer-clients)
               server-buffer-clients)
          (server-edit)
        (kill-buffer nil))
      ;; close all windows that showed this buffer
      (mapc #'(lambda (w)
                (condition-case nil
                    (delete-window w)
                  (error nil)))
            wins))))

(evil-define-command evil-quit (&optional force)
  "Close the current window, current frame, current tab, Emacs.
If the current frame belongs to some client the client connection
is closed."
  :repeat nil
  (interactive "<!>")
  (condition-case nil
      (delete-window)
    (error
     (if (and (boundp 'server-buffer-clients)
              (fboundp 'server-edit)
              (fboundp 'server-buffer-done)
              server-buffer-clients)
         (if force
             (server-buffer-done (current-buffer))
           (server-edit))
       (condition-case nil
           (delete-frame)
         (error
          (condition-case nil
              (tab-bar-close-tab)
            (error
             (if force
                 (kill-emacs)
               (save-buffers-kill-emacs))))))))))

(evil-define-command evil-quit-all (&optional bang)
  "Exit Emacs, asking for saving."
  :repeat nil
  (interactive "<!>")
  (if (null bang)
      (save-buffers-kill-terminal)
    (let ((proc (frame-parameter (selected-frame) 'client)))
      (if proc
          (with-no-warnings
            (server-delete-client proc))
        (dolist (process (process-list))
          (set-process-query-on-exit-flag process nil))
        (kill-emacs)))))

(evil-define-command evil-quit-all-with-error-code (&optional force)
  "Exit Emacs without saving, returning an non-zero error code.
The FORCE argument is only there for compatibility and is ignored.
This function fails with an error if Emacs is run in server mode."
  :repeat nil
  (interactive "<!>")
  (if (and (boundp 'server-buffer-clients)
           (fboundp 'server-edit)
           (fboundp 'server-buffer-done)
           server-buffer-clients)
      (user-error "Cannot exit client process with error code.")
    (kill-emacs 1)))

(evil-define-command evil-save-and-quit ()
  "Save all buffers and exit Emacs."
  (save-buffers-kill-terminal t))

(evil-define-command evil-save-and-close (file &optional bang)
  "Save the current buffer and close the window."
  :repeat nil
  (interactive "<f><!>")
  (evil-write nil nil nil file bang)
  (evil-quit))

(evil-define-command evil-save-modified-and-close (file &optional bang)
  "Save the current buffer and close the window."
  :repeat nil
  (interactive "<f><!>")
  (when (buffer-modified-p)
    (evil-write nil nil nil file bang))
  (evil-quit))

(evil-define-operator evil-shell-command
  (beg end type command &optional previous)
  "Execute a shell command.
If BEG, END and TYPE is specified, COMMAND is executed on the region,
which is replaced with the command's output. Otherwise, the
output is displayed in its own buffer. If PREVIOUS is non-nil,
the previous shell command is executed instead."
  (interactive "<R><sh><!>")
  (if (not (evil-ex-p))
      (let ((evil-ex-initial-input
             (if (and beg
                      (not (evil-visual-state-p))
                      (not current-prefix-arg))
                 (let ((range (evil-range beg end type)))
                   (evil-contract-range range)
                   ;; TODO: this is not exactly the same as Vim, which
                   ;; uses .,+count as range. However, this is easier
                   ;; to achieve with the current implementation and
                   ;; the very inconvenient range interface.
                   ;;
                   ;; TODO: the range interface really needs some
                   ;; rework!
                   (format
                    "%d,%d!"
                    (line-number-at-pos (evil-range-beginning range))
                    (line-number-at-pos (evil-range-end range))))
               "!")))
        (call-interactively 'evil-ex))
    (when command
      (setq command (evil-ex-replace-special-filenames command)))
    (if (zerop (length command))
        (when previous (setq command evil-previous-shell-command))
      (setq evil-previous-shell-command command))
    (cond
     ((zerop (length command))
      (if previous (user-error "No previous shell command")
        (user-error "No shell command")))
     (evil-ex-range
      (if (not evil-display-shell-error-in-message)
          (shell-command-on-region beg end command nil t)
        (let ((output-buffer (generate-new-buffer " *temp*"))
              (error-buffer (generate-new-buffer " *temp*")))
          (unwind-protect
              (if (zerop (shell-command-on-region beg end
                                                  command
                                                  output-buffer nil
                                                  error-buffer))
                  (progn
                    (delete-region beg end)
                    (insert-buffer-substring output-buffer)
                    (goto-char beg)
                    (evil-first-non-blank))
                (display-message-or-buffer error-buffer))
            (kill-buffer output-buffer)
            (kill-buffer error-buffer)))))
     (t
      (shell-command command)))))

(evil-define-command evil-make (arg)
  "Call a build command in the current directory.
If ARG is nil this function calls `recompile', otherwise it calls
`compile' passing ARG as build command."
  (interactive "<sh>")
  (if (and (fboundp 'recompile)
           (not arg))
      (recompile)
    (compile arg)))

;; TODO: escape special characters (currently only \n) ... perhaps
;; there is some Emacs function doing this?
(evil-define-command evil-show-registers (registers)
  "Show the contents of REGISTERS, or all registers, if none supplied."
  :repeat nil
  (interactive "<a>")
  (let* ((all-registers (evil-register-list))
         (reg-chars (string-to-list registers))
         (display-regs (if reg-chars
                           (cl-remove-if-not (lambda (r) (memq (car r) reg-chars))
                                             all-registers)
                         all-registers)))
    (evil-with-view-list
      :name "evil-registers"
      :mode-name "Evil Registers"
      :format
      [("Register" 10 nil)
       ("Value" 1000 nil)]
      :entries
      (cl-loop for (key . val) in display-regs
               collect `(nil [,(char-to-string key)
                              ,(cond ((stringp val)
                                      (replace-regexp-in-string "\n" "^J" val))
                                     ((vectorp val)
                                      (key-description val))
                                     (t ""))])))))

(evil-define-command evil-show-marks (mrks)
  "Show all marks.
If MRKS is non-nil it should be a string and only registers
corresponding to the characters of this string are shown."
  :repeat nil
  (interactive "<a>")
  ;; To get markers and positions, we can't rely on 'global-mark-ring'
  ;; provided by Emacs (although it will be much simpler and faster),
  ;; because 'global-mark-ring' does not store mark characters, but
  ;; only buffer name and position. Instead, 'evil-markers-alist' is
  ;; used; this is list maintained by Evil for each buffer.
  (let ((all-markers
         ;; get global and local marks
         (append (cl-remove-if (lambda (m)
                                 (or (evil-global-marker-p (car m))
                                     (not (markerp (cdr m)))))
                               evil-markers-alist)
                 (cl-remove-if (lambda (m)
                                 (or (not (evil-global-marker-p (car m)))
                                     (not (markerp (cdr m)))))
                               (default-value 'evil-markers-alist)))))
    (when mrks
      (setq mrks (string-to-list mrks))
      (setq all-markers (cl-delete-if (lambda (m)
                                        (not (member (car m) mrks)))
                                      all-markers)))
    ;; map marks to list of 4-tuples (char row col file)
    (setq all-markers
          (mapcar (lambda (m)
                    (with-current-buffer (marker-buffer (cdr m))
                      (save-excursion
                        (goto-char (cdr m))
                        (list (car m)
                              (line-number-at-pos (point))
                              (current-column)
                              (buffer-name)))))
                  all-markers))
    (evil-with-view-list
      :name "evil-marks"
      :mode-name "Evil Marks"
      :format [("Mark" 8 nil)
               ("Line" 8 nil)
               ("Column" 8 nil)
               ("Buffer" 1000 nil)]
      :entries (cl-loop for m in (sort all-markers (lambda (a b) (< (car a) (car b))))
                        collect `(nil [,(char-to-string (nth 0 m))
                                       ,(number-to-string (nth 1 m))
                                       ,(number-to-string (nth 2 m))
                                       (,(nth 3 m))]))
      :select-action #'evil--show-marks-select-action)))

(defun evil--show-marks-select-action (entry)
  (kill-buffer)
  (switch-to-buffer (car (elt entry 3)))
  (evil-goto-mark (string-to-char (elt entry 0))))

(defun evil--parse-delmarks (to-be-parsed &optional parsed)
  "Where TO-BE-PARSED can contain ranges in the form `x-y'.
PARSED is a list of characters whose marks should be deleted.
Like vim, on invalid input, preceeding valid input is still parsed."
  (cl-destructuring-bind (&optional a b c &rest) to-be-parsed
    (cond
     ((null to-be-parsed) parsed)
     ;; single mark...
     ((and (not (eq ?- b)) (or (<= ?a a ?z) (<= ?A a ?Z) (<= ?0 a ?9)
                               (memq a '(?\" ?^ ?. ?\[ ?\] ?< ?>))))
      (evil--parse-delmarks (cdr to-be-parsed) (cons a parsed)))
     ;; range of marks...
     ((and (eq ?- b) c (or (<= ?a a c ?z) (<= ?A a c ?Z) (<= ?0 a c ?9)))
      (evil--parse-delmarks (nthcdr 3 to-be-parsed)
                            (append parsed (number-sequence a c))))
     (t (progn (message "Invalid input: %s" (apply #'string (remove nil to-be-parsed)))
               parsed)))))

(evil-define-command evil-delete-marks (marks &optional force)
  "MARKS is a string denoting all marks to be deleted. Mark names are
either single characters or a range of characters in the form `x-y'.
If FORCE is non-nil and MARKS is blank, all local marks except 0-9 are removed."
  (interactive "<a><!>")
  (let ((mark-chars (remove ?\s (append marks nil))))
    (cond
     ((and force mark-chars) (message "Invalid input"))
     (mark-chars
      (let* ((delmarks (evil--parse-delmarks mark-chars))
             (delmarkp (lambda (m) (member (car m) delmarks))))
        ;; delete all parsed marks...
        (setq evil-markers-alist
              (cl-remove-if delmarkp evil-markers-alist))
        ;; ensure all parsed marks are deleted globally...
        (set-default 'evil-markers-alist
                     (cl-remove-if delmarkp (default-value 'evil-markers-alist)))))
     ;; delete local marks except 0-9...
     (force (setq evil-markers-alist
                  (cl-remove-if-not (lambda (m) (<= ?0 (car m) ?9))
                                    evil-markers-alist))))))

(eval-when-compile (require 'ffap))
(evil-define-command evil-find-file-at-point-with-line ()
  "Open the file at point and go to position if present."
  (require 'ffap)
  (let ((fname (with-no-warnings (ffap-file-at-point))))
    (unless fname
      (user-error "File does not exist."))
    (let* ((line-number-pattern ":\\([0-9]+\\)\\=" )
           (line-and-column-numbers-pattern ":\\([0-9]+\\):\\([0-9]+\\)\\=")
           (get-number (lambda (pattern match-number)
                         (save-excursion
                           (goto-char (cadr ffap-string-at-point-region))
                           (and (re-search-backward pattern (line-beginning-position) t)
                                (string-to-number (match-string match-number))))))
           (line-number (or (funcall get-number line-and-column-numbers-pattern 1)
                            (funcall get-number line-number-pattern 1)))
           (column-number (funcall get-number line-and-column-numbers-pattern 2)))
      (message "line: %s, column: %s" line-number column-number)
      (with-no-warnings (find-file-at-point fname))
      (when line-number
        (goto-char (point-min))
        (forward-line (1- line-number))
        (when column-number
          (move-to-column (1- column-number)))))))

(evil-define-command evil-find-file-at-point-visual ()
  "Find the filename selected by the visual region.
    Returns an error message if the file does not exist."
  (require 'ffap)
  (let ((region (buffer-substring (region-beginning) (region-end))))
    (if (file-exists-p region)
        (find-file-at-point region)
      (user-error (format "Can't find file \"%s\" in path" region)))))

(evil-ex-define-argument-type state
  "Define an argument type which can take state names."
  :collection
  (lambda (arg predicate flag)
    (let ((completions
           (append '("nil")
                   (mapcar #'(lambda (state)
                               (format "%s" (car state)))
                           evil-state-properties))))
      (when arg
        (cond
         ((eq flag nil)
          (try-completion arg completions predicate))
         ((eq flag t)
          (all-completions arg completions predicate))
         ((eq flag 'lambda)
          (test-completion arg completions predicate))
         ((eq (car-safe flag) 'boundaries)
          (cons 'boundaries
                (completion-boundaries arg
                                       completions
                                       predicate
                                       (cdr flag)))))))))

(evil-define-interactive-code "<state>"
  "A valid evil state."
  :ex-arg state
  (list (when (and (evil-ex-p) evil-ex-argument)
          (intern evil-ex-argument))))

;; TODO: should we merge this command with `evil-set-initial-state'?
(evil-define-command evil-ex-set-initial-state (state)
  "Set the initial state for the current major mode to STATE.
This is the state the buffer comes up in. See `evil-set-initial-state'."
  :repeat nil
  (interactive "<state>")
  (if (not (or (assq state evil-state-properties)
               (null state)))
      (user-error "State %s cannot be set as initial Evil state" state)
    (let ((current-initial-state (evil-initial-state major-mode)))
      (unless (eq current-initial-state state)
        ;; only if we selected a new mode
        (when (y-or-n-p (format "Major-mode `%s' has initial mode `%s'. \
Change to `%s'? "
                                major-mode
                                (or current-initial-state "DEFAULT")
                                (or state "DEFAULT")))
          (evil-set-initial-state major-mode state)
          (when (y-or-n-p "Save setting in customization file? ")
            (dolist (s (list current-initial-state state))
              (when s
                (let ((var (intern (format "evil-%s-state-modes" s))))
                  (customize-save-variable var (symbol-value var)))))))))))

(evil-define-command evil-force-normal-state ()
  "Switch to normal state without recording current command."
  :repeat abort
  :suppress-operator t
  (evil-normal-state))

(evil-define-motion evil-ex-search-next (count)
  "Go to the next occurrence."
  :jump t
  :type exclusive
  (evil-ex-search count))

(evil-define-motion evil-ex-search-previous (count)
  "Go the the previous occurrence."
  :jump t
  :type exclusive
  (let ((evil-ex-search-direction
         (if (eq evil-ex-search-direction 'backward) 'forward 'backward)))
    (evil-ex-search count)))

(defun evil-repeat-ex-search (flag)
  "Called to record a search command.
FLAG is either 'pre or 'post if the function is called before
resp.  after executing the command."
  (cond
   ((and (evil-operator-state-p) (eq flag 'pre))
    (evil-repeat-record (this-command-keys))
    (evil-clear-command-keys))
   ((and (evil-operator-state-p) (eq flag 'post))
    (evil-repeat-record (evil-ex-pattern-regex evil-ex-search-pattern))
    ;; If it weren't for the fact that `exit-minibuffer' throws an `exit'
    ;; tag, which bypasses the source of `this-command-keys', we'd be able
    ;; to capture the key(s) in the post-command of the operator as usual.
    ;; Fortunately however, `last-input-event' can see the key (by default, `return')
    (unless (append (this-command-keys) nil)
      (evil-repeat-record (vector last-input-event))))
   (t (evil-repeat-motion flag))))

(evil-define-motion evil-ex-search-forward (count)
  "Start a forward search."
  :jump t
  :type exclusive
  :repeat evil-repeat-ex-search
  (evil-ex-start-search 'forward count))

(evil-define-motion evil-ex-search-backward (count)
  "Start a forward search."
  :jump t
  :repeat evil-repeat-ex-search
  (evil-ex-start-search 'backward count))

(evil-define-motion evil-ex-search-word-forward (count &optional symbol)
  "Search for the next occurrence of word under the cursor."
  :jump t
  :type exclusive
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     evil-symbol-word-search))
  (evil-ex-start-word-search nil 'forward count symbol))

(evil-define-motion evil-ex-search-word-backward (count &optional symbol)
  "Search for the next occurrence of word under the cursor."
  :jump t
  :type exclusive
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     evil-symbol-word-search))
  (evil-ex-start-word-search nil 'backward count symbol))

(evil-define-motion evil-ex-search-unbounded-word-forward (count &optional symbol)
  "Search for the next occurrence of word under the cursor."
  :jump t
  :type exclusive
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     evil-symbol-word-search))
  (evil-ex-start-word-search t 'forward count symbol))

(evil-define-motion evil-ex-search-unbounded-word-backward (count &optional symbol)
  "Search for the next occurrence of word under the cursor."
  :jump t
  :type exclusive
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     evil-symbol-word-search))
  (evil-ex-start-word-search t 'backward count symbol))

(defun evil-revert-reveal (open-spots)
  "Unconditionally close overlays in OPEN-SPOTS in current window.
Modified version of `reveal-close-old-overlays' from
reveal.el. OPEN-SPOTS is a local version of `reveal-open-spots'."
  (dolist (spot open-spots)
    (let ((window (car spot))
          (ol (cdr spot)))
      (unless (eq window (selected-window))
        (error "evil-revert-reveal: slot with wrong window"))
      (let* ((inv (overlay-get ol 'reveal-invisible))
             (open (or (overlay-get ol 'reveal-toggle-invisible)
                       (get inv 'reveal-toggle-invisible)
                       (overlay-get ol 'isearch-open-invisible-temporary))))
        (if (and (overlay-start ol) ;Check it's still live.
                 open)
            (condition-case err
                (funcall open ol t)
              (error (message "!!Reveal-hide (funcall %s %s t): %s !!"
                              open ol err)))
          (overlay-put ol 'invisible inv))
        ;; Remove the overlay from the list of open spots.
        (overlay-put ol 'reveal-invisible nil)))))

(evil-define-operator evil-ex-substitute
  (beg end pattern replacement flags)
  "The Ex substitute command.
\[BEG,END]substitute/PATTERN/REPLACEMENT/FLAGS"
  :repeat nil
  :jump t
  :move-point nil
  :motion evil-line
  (interactive "<r><s/>")
  (evil-ex-nohighlight)
  (unless pattern
    (user-error "No pattern given"))
  (setq replacement (or replacement ""))
  (setq evil-ex-last-was-search nil)
  (let* ((flags (append flags nil))
         (count-only (memq ?n flags))
         (confirm (and (memq ?c flags) (not count-only)))
         (case-fold-search (evil-ex-pattern-ignore-case pattern))
         (case-replace case-fold-search)
         (evil-ex-substitute-regex (evil-ex-pattern-regex pattern))
         (evil-ex-substitute-nreplaced 0)
         (evil-ex-substitute-last-point (point))
         (whole-line (evil-ex-pattern-whole-line pattern))
         (evil-ex-substitute-overlay (make-overlay (point) (point)))
         (orig-point-marker (move-marker (make-marker) (point)))
         (end-marker (move-marker (make-marker) end))
         (use-reveal confirm)
         (match-end (make-marker))
         reveal-open-spots
         transient-mark-mode)
    (setq evil-ex-substitute-pattern pattern
          evil-ex-substitute-replacement replacement
          evil-ex-substitute-flags flags
          isearch-string evil-ex-substitute-regex)
    (isearch-update-ring evil-ex-substitute-regex t)
    (unwind-protect
        (progn
          (evil-ex-hl-change 'evil-ex-substitute pattern)
          (overlay-put evil-ex-substitute-overlay 'face 'isearch)
          (overlay-put evil-ex-substitute-overlay 'priority 1001)
          (goto-char beg)
          (catch 'exit-search
            (while (re-search-forward evil-ex-substitute-regex end-marker t)
              (unless (and query-replace-skip-read-only
                           (text-property-any (match-beginning 0) (match-end 0) 'read-only t))
                (let ((inhibit-field-text-motion t)
                      (match-beg (match-beginning 0))
                      (match-data (match-data))
                      match-contains-newline zero-length-match)
                  (move-marker match-end (match-end 0))
                  (goto-char match-beg)
                  (setq match-contains-newline (< (line-end-position) match-end)
                        zero-length-match (= match-beg match-end))
                  (when (and (= match-beg end-marker) (> end-marker beg) (bolp))
                    ;; This line is not included due to range being exclusive
                    (throw 'exit-search t))
                  (setq evil-ex-substitute-last-point match-beg)
                  (if confirm
                      (let ((prompt
                             (format "Replace %s with %s (y/n/a/q/l/^E/^Y)? "
                                     (match-string 0)
                                     (evil-match-substitute-replacement
                                      evil-ex-substitute-replacement
                                      (not case-replace))))
                            (search-invisible t)
                            response)
                        (move-overlay evil-ex-substitute-overlay match-beg match-end)
                        ;; Simulate `reveal-mode'. `reveal-mode' uses
                        ;; `post-command-hook' but that won't work here.
                        (when use-reveal
                          (reveal-post-command))
                        (catch 'exit-read-char
                          (while (setq response (read-char prompt))
                            (when (member response '(?y ?a ?l))
                              (unless count-only
                                (set-match-data match-data)
                                (evil-replace-match evil-ex-substitute-replacement
                                                    (not case-replace)))
                              (setq evil-ex-substitute-nreplaced
                                    (1+ evil-ex-substitute-nreplaced))
                              (evil-ex-hl-set-region 'evil-ex-substitute
                                                     (save-excursion
                                                       (forward-line)
                                                       (point))
                                                     (evil-ex-hl-get-max
                                                      'evil-ex-substitute)))
                            (cl-case response
                              ((?y ?n) (throw 'exit-read-char t))
                              (?a (setq confirm nil)
                                  (throw 'exit-read-char t))
                              ((?q ?l ?\C-\[) (throw 'exit-search t))
                              (?\C-e (evil-scroll-line-down 1))
                              (?\C-y (evil-scroll-line-up 1))))))
                    (setq evil-ex-substitute-nreplaced
                          (1+ evil-ex-substitute-nreplaced))
                    (unless count-only
                      (set-match-data match-data)
                      (evil-replace-match evil-ex-substitute-replacement
                                          (not case-replace))))
                  (goto-char match-end)
                  (cond ((>= (point) end-marker)
                         ;; Don't want to perform multiple replacements at the end
                         ;; of the search region.
                         (throw 'exit-search t))
                        ((and (not whole-line)
                              (not match-contains-newline))
                         (forward-line)
                         ;; forward-line just moves to the end of the line on the
                         ;; last line of the buffer.
                         (when (or (eobp)
                                   (> (point) end-marker))
                           (throw 'exit-search t)))
                        ;; For zero-length matches check to see if point won't
                        ;; move next time. This is a problem when matching the
                        ;; regexp "$" because we can enter an infinite loop,
                        ;; repeatedly matching the same character
                        ((and zero-length-match
                              (let ((pnt (point)))
                                (save-excursion
                                  (and
                                   (re-search-forward
                                    evil-ex-substitute-regex end-marker t)
                                   (= pnt (point))))))
                         (if (or (eobp)
                                 (>= (point) end-marker))
                             (throw 'exit-search t)
                           (forward-char)))))))))
      (evil-ex-delete-hl 'evil-ex-substitute)
      (delete-overlay evil-ex-substitute-overlay)

      (if count-only
          (goto-char orig-point-marker)
        (goto-char evil-ex-substitute-last-point))

      (move-marker orig-point-marker nil)
      (move-marker end-marker nil)

      (when use-reveal
        (evil-revert-reveal reveal-open-spots)))

    (message "%s %d occurrence%s"
             (if count-only "Found" "Replaced")
             evil-ex-substitute-nreplaced
             (if (/= evil-ex-substitute-nreplaced 1) "s" ""))
    (if (and (= 0 evil-ex-substitute-nreplaced) evil-ex-point)
        (goto-char evil-ex-point)
      (evil-first-non-blank))))

(evil-define-operator evil-ex-repeat-substitute
  (beg end flags)
  "Repeat last substitute command.
This is the same as :s//~/"
  :repeat nil
  :jump t
  :move-point nil
  :motion evil-line
  (interactive "<r><a>")
  (apply #'evil-ex-substitute beg end
         (evil-ex-get-substitute-info (concat "//~/" flags))))

(evil-define-operator evil-ex-repeat-substitute-with-flags
  (beg end flags)
  "Repeat last substitute command with last flags.
This is the same as :s//~/&"
  :repeat nil
  :jump t
  :move-point nil
  :motion evil-line
  (interactive "<r><a>")
  (apply #'evil-ex-substitute beg end
         (evil-ex-get-substitute-info (concat "//~/&" flags))))

(evil-define-operator evil-ex-repeat-substitute-with-search
  (beg end flags)
  "Repeat last substitute command with last search pattern.
This is the same as :s//~/r"
  :repeat nil
  :jump t
  :move-point nil
  :motion evil-line
  (interactive "<r><a>")
  (apply #'evil-ex-substitute beg end
         (evil-ex-get-substitute-info (concat "//~/r" flags))))

(evil-define-operator evil-ex-repeat-substitute-with-search-and-flags
  (beg end flags)
  "Repeat last substitute command with last search pattern and last flags.
This is the same as :s//~/&r"
  :repeat nil
  :jump t
  :move-point nil
  :motion evil-line
  (interactive "<r><a>")
  (apply #'evil-ex-substitute beg end
         (evil-ex-get-substitute-info (concat "//~/&r" flags))))

(evil-define-operator evil-ex-repeat-global-substitute ()
  "Repeat last substitute command on the whole buffer.
This is the same as :%s//~/&"
  :repeat nil
  :jump t
  :move-point nil
  :motion evil-line
  (interactive)
  (apply #'evil-ex-substitute (point-min) (point-max)
         (evil-ex-get-substitute-info (concat "//~/&"))))

(defun evil-keep-lines (pattern beg end)
  "Stripped down version of `keep-lines'.
Delete lines between BEG & END which don't match PATTERN."
  (goto-char (min beg end))
  (setq end
        (progn
          (save-excursion
            (goto-char (max beg end))
            (unless (or (bolp) (eobp))
              (forward-line 0))
            (point-marker))))
  (save-excursion
    (or (bolp) (forward-line 1))
    (let ((start (point)))
      (while (< (point) end)
        ;; Start is first char not preserved by previous match.
        (if (not (re-search-forward pattern end 'move))
            (delete-region start end)
          (let ((end (save-excursion (goto-char (match-beginning 0))
                                     (forward-line 0)
                                     (point))))
            ;; Now end is first char preserved by the new match.
            (if (< start end)
                (delete-region start end))))

        (setq start (save-excursion (forward-line 1) (point)))
        ;; If the match was empty, avoid matching again at same place.
        (and (< (point) end)
             (= (match-beginning 0) (match-end 0))
             (forward-char 1)))))
  (set-marker end nil)
  nil)

(defun evil-flush-lines (pattern beg end)
  "Stripped down version of `flush-lines'.
Delete lines between BEG & END which match PATTERN."
  (goto-char (min beg end))
  (setq end (copy-marker (max beg end)))
  (save-excursion
    (while (and (< (point) end)
                (re-search-forward pattern end t))
      (delete-region (save-excursion (goto-char (match-beginning 0))
                                     (forward-line 0)
                                     (point))
                     (progn (forward-line 1) (point)))))
  (set-marker end nil))

(defun evil--ex-performant-global-delete (beg end pattern invert)
  "Use fast functions for fast line deletion.
Delete lines between BEG & END which match PATTERN.
Use `evil-flush-lines' if INVERT is nil, or `evil-keep-lines' if not."
  (goto-char end)
  (re-search-backward pattern beg t)
  (let ((end-marker (make-marker)))
    (set-marker end-marker (point))
    (if invert
        (evil-keep-lines pattern beg end)
      (evil-flush-lines pattern beg end))
    (goto-char end-marker)
    (set-marker end-marker nil)))

(evil-define-operator evil-ex-global
  (beg end pattern command &optional invert)
  "The Ex global command.
\[BEG,END]global[!]/PATTERN/COMMAND"
  :motion mark-whole-buffer
  :move-point nil
  (interactive "<r><g/><!>")
  (unless pattern
    (user-error "No pattern given"))
  (unless command
    (user-error "No command given"))
  ;; TODO: `evil-ex-make-substitute-pattern' should be executed so
  ;; :substitute can re-use :global's pattern depending on its `r'
  ;; flag. This isn't supported currently but should be simple to add
  (evil-with-single-undo
    (let* ((case-fold-search
            (eq (evil-ex-regex-case pattern evil-ex-search-case) 'insensitive))
           (pattern (evil-ex-regex-without-case pattern))
           (command-form (evil-ex-parse command))
           (ex-delete (eq 'evil-ex-delete (evil-ex-completed-binding (nth 2 command-form))))
           (transient-mark-mode transient-mark-mode)
           (deactivate-mark deactivate-mark)
           match markers)
      (when (and pattern command)
        (when evil-ex-search-vim-style-regexp
          (setq pattern (evil-transform-vim-style-regexp pattern)))
        (if (and ex-delete (not (nth 3 command-form)))
            (evil--ex-performant-global-delete beg end pattern invert)
          (setq isearch-string pattern)
          (isearch-update-ring pattern t)
          (goto-char beg)
          (evil-move-beginning-of-line)
          (while (< (point) end)
            (setq match (re-search-forward pattern (line-end-position) t))
            (when (or (and match (not invert))
                      (and invert (not match)))
              (push (move-marker (make-marker)
                                 (or (and match (match-beginning 0))
                                     (line-beginning-position)))
                    markers))
            (forward-line))
          (setq markers (nreverse markers))
          (unwind-protect
              (dolist (marker markers)
                (goto-char marker)
                (eval command-form))
            ;; ensure that all markers are deleted afterwards,
            ;; even in the event of failure
            (dolist (marker markers)
              (set-marker marker nil))))))))

(evil-define-operator evil-ex-global-inverted
  (beg end pattern command &optional invert)
  "The Ex vglobal command.
\[BEG,END]vglobal/PATTERN/COMMAND"
  :motion mark-whole-buffer
  :move-point nil
  (interactive "<r><g/><!>")
  (evil-ex-global beg end pattern command (not invert)))

(evil-define-operator evil-ex-normal (beg end commands)
  "The Ex normal command.
Execute the argument as normal command on each line in the
range. The given argument is passed straight to
`execute-kbd-macro'.  The default is the current line."
  :motion evil-line
  (interactive "<r><a>")
  (evil-with-single-undo
    (let (markers evil-ex-current-buffer prefix-arg current-prefix-arg)
      (goto-char beg)
      (while
          (and (< (point) end)
               (progn
                 (push (move-marker (make-marker) (line-beginning-position))
                       markers)
                 (and (= (forward-line) 0) (bolp)))))
      (setq markers (nreverse markers))
      (deactivate-mark)
      (evil-force-normal-state)
      ;; replace ^[ by escape
      (setq commands
            (vconcat
             (mapcar #'(lambda (ch) (if (equal ch ?) 'escape ch))
                     (append commands nil))))
      (dolist (marker markers)
        (goto-char marker)
        (condition-case nil
            (execute-kbd-macro commands)
          (error nil))
        (evil-force-normal-state)
        (set-marker marker nil)))))

(evil-define-command evil-goto-char (position)
  "Go to POSITION in the buffer.
Default position is the beginning of the buffer."
  :jump t
  (interactive "<N>")
  (goto-char (or position (point-min))))

(evil-define-operator evil-ex-line-number (beg end)
  "Print the last line number."
  :motion mark-whole-buffer
  :move-point nil
  (interactive "<r>")
  (message "%d" (count-lines (point-min) end)))

(evil-define-command evil-show-file-info ()
  "Show basic file information."
  (let* ((nlines   (count-lines (point-min) (point-max)))
         (curr     (line-number-at-pos (point)))
         (perc     (if (> nlines 0)
                       (format "%d%%" (* (/ (float curr) (float nlines)) 100.0))
                     "No lines in buffer"))
         (file     (buffer-file-name (buffer-base-buffer)))
         (writable (and file (file-writable-p file)))
         (readonly (if (and file (not writable)) "[readonly] " "")))
    (if file
        (message "\"%s\" %d %slines --%s--" file nlines readonly perc)
      (message "%d lines --%s--" nlines perc))))

(defvar sort-fold-case)
(evil-define-operator evil-ex-sort (beg end &optional options reverse)
  "The Ex sort command.
\[BEG,END]sort[!] [i][u]
The following additional options are supported:

  * i   ignore case
  * u   remove duplicate lines

The 'bang' argument means to sort in reverse order."
  :motion mark-whole-buffer
  :move-point nil
  (interactive "<r><a><!>")
  (let ((beg (copy-marker beg))
        (end (copy-marker end))
        sort-fold-case uniq)
    (dolist (opt (append options nil))
      (cond
       ((eq opt ?i) (setq sort-fold-case t))
       ((eq opt ?u) (setq uniq t))
       (t (user-error "Unsupported sort option: %c" opt))))
    (sort-lines reverse beg end)
    (when uniq
      (let (line prev-line)
        (goto-char beg)
        (while (and (< (point) end) (not (eobp)))
          (setq line (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position)))
          (if (and (stringp prev-line)
                   (eq t (compare-strings line nil nil
                                          prev-line nil nil
                                          sort-fold-case)))
              (delete-region (progn (forward-line 0) (point))
                             (progn (forward-line 1) (point)))
            (setq prev-line line)
            (forward-line 1)))))
    (goto-char beg)
    (set-marker beg nil)
    (set-marker end nil)))

;;; Window navigation

(defmacro evil-save-side-windows (&rest body)
  "Toggle side windows, evaluate BODY, restore side windows."
  (declare (indent defun) (debug (&rest form)))
  (let ((sides (make-symbol "sidesvar")))
    `(let ((,sides (and (functionp 'window-toggle-side-windows)
                        (window-with-parameter 'window-side))))
       (when ,sides
         (window-toggle-side-windows))
       (unwind-protect
           (progn ,@body)
         (when ,sides
           (window-toggle-side-windows))))))

(defun evil-resize-window (new-size &optional horizontal)
  "Set the current window's width or height to NEW-SIZE.
If HORIZONTAL is non-nil the width of the window is changed,
otherwise its height is changed."
  (let ((count (- new-size (if horizontal (window-width) (window-height)))))
    (enlarge-window count horizontal)))

(defun evil-move-window (side)
  "Move the `selected-window' to SIDE.
The state of the `selected-window' is saved along with the state
of the window tree consisting of all the other windows. Then, all
windows are deleted, the remaining window is split according to
SIDE, the state of the window at SIDE is replaced with the saved
state of the `selected-window', and, finally, the state of the
saved window tree is reconstructed on the opposite side.

SIDE has the same meaning as in `split-window'.

Note, this function only operates on the window tree rooted in
the frame's main window and effectively preserves any side
windows (i.e. windows with a valid window-side window
parameter)."
  (evil-save-side-windows
    (unless (one-window-p)
      (save-excursion
        (let ((w (window-state-get (selected-window))))
          (delete-window)
          (let ((wtree (window-state-get)))
            (delete-other-windows)
            (let ((subwin (selected-window))
                  ;; NOTE: SIDE is new in Emacs 24
                  (newwin (split-window nil nil side)))
              (window-state-put wtree subwin)
              (window-state-put w newwin)
              (select-window newwin)))))
      (balance-windows))))

(defun evil-alternate-buffer (&optional window)
  "Return the last buffer WINDOW has displayed other than the current one.
This is equivalent to Vim's alternate buffer."
  ;; If the last buffer visited has been killed, then `window-prev-buffers'
  ;; returns a list with `window-buffer' at the head.
  (let* ((prev-buffers (window-prev-buffers))
         (head (car prev-buffers)))
    (if (eq (car head) (window-buffer window))
        (cadr prev-buffers)
      head)))

(evil-define-command evil-switch-to-windows-last-buffer ()
  "Switch to the last open buffer of the current window."
  :repeat nil
  (let ((previous-place (evil-alternate-buffer)))
    (when previous-place
      (switch-to-buffer (car previous-place)))))

(evil-define-command evil-window-delete ()
  "Delete the current window.
If `evil-auto-balance-windows' is non-nil then all children of
the deleted window's parent window are rebalanced."
  (let ((p (window-parent)))
    (delete-window)
    (when evil-auto-balance-windows
      ;; balance-windows raises an error if the parent does not have
      ;; any further children (then rebalancing is not necessary anyway)
      (condition-case nil
          (balance-windows p)
        (error)))))

(evil-define-command evil-window-split (&optional count file)
  "Split the current window horizontally, COUNT lines height,
editing a certain FILE. The new window will be created below
when `evil-split-window-below' is non-nil. If COUNT and
`evil-auto-balance-windows' are both non-nil then all children
of the parent of the splitted window are rebalanced."
  :repeat nil
  (interactive "P<f>")
  (select-window
   (split-window (selected-window) (when count (- count))
                 (if evil-split-window-below 'below 'above)))
  (when (and (not count) evil-auto-balance-windows)
    (balance-windows (window-parent)))
  (when file
    (evil-edit file)))

(evil-define-command evil-window-vsplit (&optional count file)
  "Split the current window vertically, COUNT columns width,
editing a certain FILE. The new window will be created to the
right when `evil-vsplit-window-right' is non-nil. If COUNT and
`evil-auto-balance-windows'are both non-nil then all children
of the parent of the splitted window are rebalanced."
  :repeat nil
  (interactive "P<f>")
  (select-window
   (split-window (selected-window) (when count (- count))
                 (if evil-vsplit-window-right 'right 'left)))
  (when (and (not count) evil-auto-balance-windows)
    (balance-windows (window-parent)))
  (when file
    (evil-edit file)))

(evil-define-command evil-split-buffer (buffer)
  "Split window and switch to another buffer."
  :repeat nil
  (interactive "<b>")
  (evil-window-split)
  (evil-buffer buffer))

(evil-define-command evil-split-next-buffer (&optional count)
  "Split the window and go to the COUNT-th next buffer in the buffer list."
  :repeat nil
  (interactive "p")
  (evil-window-split)
  (evil-next-buffer count))

(evil-define-command evil-split-prev-buffer (&optional count)
  "Split window and go to the COUNT-th prev buffer in the buffer list."
  :repeat nil
  (interactive "p")
  (evil-window-split)
  (evil-prev-buffer count))

(evil-define-command evil-window-left (count)
  "Move the cursor to new COUNT-th window left of the current one."
  :repeat nil
  (interactive "p")
  (dotimes (_ count)
    (windmove-left)))

(evil-define-command evil-window-right (count)
  "Move the cursor to new COUNT-th window right of the current one."
  :repeat nil
  (interactive "p")
  (dotimes (_ count)
    (windmove-right)))

(evil-define-command evil-window-up (count)
  "Move the cursor to new COUNT-th window above the current one."
  :repeat nil
  (interactive "p")
  (dotimes (_ (or count 1))
    (windmove-up)))

(evil-define-command evil-window-down (count)
  "Move the cursor to new COUNT-th window below the current one."
  :repeat nil
  (interactive "p")
  (dotimes (_ (or count 1))
    (windmove-down)))

(evil-define-command evil-window-bottom-right ()
  "Move the cursor to bottom-right window."
  :repeat nil
  (let ((last-sibling (frame-root-window)))
    (while (and last-sibling (not (window-live-p last-sibling)))
      (setq last-sibling (window-last-child last-sibling)))
    (when last-sibling
      (select-window last-sibling))))

(evil-define-command evil-window-top-left ()
  "Move the cursor to top-left window."
  :repeat nil
  (let ((first-child (window-child (frame-root-window))))
    (while (and first-child (not (window-live-p first-child)))
      (setq first-child (window-child first-child)))
    (when first-child
      (select-window
       first-child))))

(evil-define-command evil-window-mru ()
  "Move the cursor to the previous (last accessed) buffer in another window.
More precisely, it selects the most recently used buffer that is
shown in some other window, preferably of the current frame, and
is different from the current one."
  :repeat nil
  (catch 'done
    (dolist (buf (buffer-list (selected-frame)))
      (let ((win (get-buffer-window buf)))
        (when (and (not (eq buf (current-buffer)))
                   win
                   (not (eq win (selected-window))))
          (select-window win)
          (throw 'done nil))))))

(evil-define-command evil-window-next (count)
  "Move the cursor to the next window in the cyclic order.
With COUNT go to the count-th window in the order starting from
top-left."
  :repeat nil
  (interactive "<c>")
  (if (not count)
      (select-window (next-window))
    (evil-window-top-left)
    (other-window (1- (min count (length (window-list)))))))

(evil-define-command evil-window-prev (count)
  "Move the cursor to the previous window in the cyclic order.
With COUNT go to the count-th window in the order starting from
top-left."
  :repeat nil
  (interactive "<c>")
  (if (not count)
      (select-window (previous-window))
    (evil-window-top-left)
    (other-window (1- (min count (length (window-list)))))))

(evil-define-command evil-window-new (count file)
  "Split the current window horizontally
and open a new buffer or edit a certain FILE."
  :repeat nil
  (interactive "P<f>")
  (let ((new-window (split-window (selected-window) (when count (- count))
                                  (if evil-split-window-below 'below 'above))))
    (when (and (not count) evil-auto-balance-windows)
      (balance-windows (window-parent)))
    (let ((buffer (generate-new-buffer "*new*")))
      (set-window-buffer new-window buffer)
      (select-window new-window)
      (with-current-buffer buffer
        (funcall (default-value 'major-mode))))
    (when file
      (evil-edit file))))

(evil-define-command evil-window-vnew (count file)
  "Split the current window vertically
and open a new buffer name or edit a certain FILE."
  :repeat nil
  (interactive "P<f>")
  (let ((new-window (split-window (selected-window) (when count (- count))
                                  (if evil-vsplit-window-right 'right 'left))))
    (when (and (not count) evil-auto-balance-windows)
      (balance-windows (window-parent)))
    (let ((buffer (generate-new-buffer "*new*")))
      (set-window-buffer new-window buffer)
      (select-window new-window)
      (with-current-buffer buffer
        (funcall (default-value 'major-mode))))
    (when file
      (evil-edit file))))

(evil-define-command evil-buffer-new (count file)
  "Create a new buffer replacing the current window, optionally
   editing a certain FILE"
  :repeat nil
  (interactive "P<f>")
  (if file
      (evil-edit file)
    (let ((buffer (generate-new-buffer "*new*")))
      (set-window-buffer nil buffer)
      (with-current-buffer buffer
        (funcall (default-value 'major-mode))))))

(evil-define-command evil-window-increase-height (count)
  "Increase current window height by COUNT."
  :repeat nil
  (interactive "p")
  (evil-resize-window (+ (window-height) count)))

(evil-define-command evil-window-decrease-height (count)
  "Decrease current window height by COUNT."
  :repeat nil
  (interactive "p")
  (evil-resize-window (- (window-height) count)))

(evil-define-command evil-window-increase-width (count)
  "Increase current window width by COUNT."
  :repeat nil
  (interactive "p")
  (evil-resize-window (+ (window-width) count) t))

(evil-define-command evil-window-decrease-width (count)
  "Decrease current window width by COUNT."
  :repeat nil
  (interactive "p")
  (evil-resize-window (- (window-width) count) t))

(evil-define-command evil-window-set-height (count)
  "Set the height of the current window to COUNT."
  :repeat nil
  (interactive "<c>")
  (evil-resize-window (or count (frame-height)) nil))

(evil-define-command evil-window-set-width (count)
  "Set the width of the current window to COUNT."
  :repeat nil
  (interactive "<c>")
  (evil-resize-window (or count (frame-width)) t))

(evil-define-command evil-ex-resize (arg)
  "The ex :resize command.

If ARG is a signed positive integer, increase the current window
height by ARG.

If ARG is a signed negative integer, decrease the current window
height by ARG.

If ARG is a positive integer without explicit sign, set the current
window height to ARG.

If ARG is empty, maximize the current window height."
  (interactive "<a>")
  (if (or (not arg) (= 0 (length arg)))
      (evil-window-set-height nil)
    (let ((n (string-to-number arg)))
      (if (> n 0)
          (if (= ?+ (aref arg 0))
              (evil-window-increase-height n)
            (evil-window-set-height n))
        (evil-window-decrease-height (- n))))))

(evil-define-command evil-window-rotate-upwards ()
  "Rotate the windows according to the current cyclic ordering."
  :repeat nil
  (evil-save-side-windows
    (let ((wlist (window-list))
          (slist (mapcar #'window-state-get (window-list))))
      (setq slist (append (cdr slist) (list (car slist))))
      (while (and wlist slist)
        (window-state-put (car slist) (car wlist))
        (setq wlist (cdr wlist)
              slist (cdr slist)))
      (select-window (cadr (last (window-list)))))))

(evil-define-command evil-window-rotate-downwards ()
  "Rotate the windows according to the current cyclic ordering."
  :repeat nil
  (evil-save-side-windows
    (let ((wlist (window-list))
          (slist (mapcar #'window-state-get (window-list))))
      (setq slist (append (last slist) slist))
      (while (and wlist slist)
        (window-state-put (car slist) (car wlist))
        (setq wlist (cdr wlist)
              slist (cdr slist)))
      (select-window (car (window-list))))))

(evil-define-command evil-window-exchange (count)
  "Without COUNT: exchange current window with next one.
With COUNT: Exchange current window with COUNTth window."
  :repeat nil
  (interactive "<c>")
  (let ((this-buffer (window-buffer))
        other-buffer other-window)
    (if (not count)
        (setq other-buffer (window-buffer (next-window))
              other-window (next-window))
      (save-window-excursion
        (evil-window-next count)
        (setq other-buffer (window-buffer)
              other-window (selected-window))))
    (switch-to-buffer other-buffer nil t)
    (select-window other-window)
    (switch-to-buffer this-buffer nil t)))

(evil-define-command evil-window-move-very-top ()
  "Close the current window, split the upper-left one horizontally
and redisplay the current buffer there."
  :repeat nil
  (evil-move-window 'above))

(evil-define-command evil-window-move-far-left ()
  "Close the current window, split the upper-left one vertically
and redisplay the current buffer there."
  :repeat nil
  (evil-move-window 'left))

(evil-define-command evil-window-move-far-right ()
  "Close the current window, split the lower-right one vertically
and redisplay the current buffer there."
  :repeat nil
  (evil-move-window 'right))

(evil-define-command evil-window-move-very-bottom ()
  "Close the current window, split the lower-right one horizontally
and redisplay the current buffer there."
  :repeat nil
  (evil-move-window 'below))

;;; Mouse handling

;; Large parts of this code are taken from mouse.el which is
;; distributed with GNU Emacs
(defun evil-mouse-drag-region (start-event)
  "Set the region to the text that the mouse is dragged over.
Highlight the drag area as you move the mouse.
This must be bound to a button-down mouse event.

If the click is in the echo area, display the `*Messages*' buffer.

START-EVENT should be the event that started the drag."
  (interactive "e")
  ;; Give temporary modes such as isearch a chance to turn off.
  (run-hooks 'mouse-leave-buffer-hook)
  (evil-mouse-drag-track start-event t))
(evil-set-command-property 'evil-mouse-drag-region :keep-visual t)

(defun evil-mouse-drag-track (start-event &optional
                                          do-mouse-drag-region-post-process)
  "Track mouse drags by highlighting area between point and cursor.
The region will be defined with mark and point.
DO-MOUSE-DRAG-REGION-POST-PROCESS should only be used by
`mouse-drag-region'."
  (mouse-minibuffer-check start-event)
  (setq mouse-selection-click-count-buffer (current-buffer))
  (deactivate-mark)
  (let* ((scroll-margin 0) ; Avoid margin scrolling (Bug#9541).
         (original-window (selected-window))
         ;; We've recorded what we needed from the current buffer and
         ;; window, now let's jump to the place of the event, where things
         ;; are happening.
         (_ (mouse-set-point start-event))
         (echo-keystrokes 0)
         (start-posn (event-start start-event))
         (start-point (posn-point start-posn))
         (start-window (posn-window start-posn))
         (start-window-start (window-start start-window))
         (start-hscroll (window-hscroll start-window))
         (bounds (window-edges start-window))
         (make-cursor-line-fully-visible nil)
         (top (nth 1 bounds))
         (bottom (if (or (window-minibuffer-p start-window)
                         (not mode-line-format))
                     (nth 3 bounds)
                   ;; Don't count the mode line.
                   (1- (nth 3 bounds))))
         (on-link (and mouse-1-click-follows-link
                       (or mouse-1-click-in-non-selected-windows
                           (eq start-window original-window))
                       ;; Use start-point before the intangibility
                       ;; treatment, in case we click on a link inside an
                       ;; intangible text.
                       (mouse-on-link-p start-posn)))
         (click-count (1- (event-click-count start-event)))
         (remap-double-click (and on-link
                                  (eq mouse-1-click-follows-link 'double)
                                  (= click-count 1)))
         ;; Suppress automatic hscrolling, because that is a nuisance
         ;; when setting point near the right fringe (but see below).
         (auto-hscroll-mode-saved auto-hscroll-mode)
         (auto-hscroll-mode nil)
         event end end-point)

    (setq mouse-selection-click-count click-count)
    ;; In case the down click is in the middle of some intangible text,
    ;; use the end of that text, and put it in START-POINT.
    (if (< (point) start-point)
        (goto-char start-point))
    (setq start-point (point))
    (if remap-double-click
        (setq click-count 0))

    (setq click-count (mod click-count 4))

    ;; activate correct visual state
    (let ((range (evil-mouse-start-end start-point start-point click-count)))
      (set-mark (nth 0 range))
      (goto-char (nth 1 range)))

    (cond
     ((= click-count 0)
      (when (evil-visual-state-p) (evil-exit-visual-state)))
     ((= click-count 1)
      (evil-visual-char)
      (evil-visual-post-command))
     ((= click-count 2)
      (evil-visual-line)
      (evil-visual-post-command))
     ((= click-count 3)
      (evil-visual-block)
      (evil-visual-post-command)))

    ;; Track the mouse until we get a non-movement event.
    (track-mouse
      (while (progn
               (setq event (read-key))
               (or (mouse-movement-p event)
                   (memq (car-safe event) '(switch-frame select-window))))
        (unless (evil-visual-state-p)
          (cond
           ((= click-count 0) (evil-visual-char))
           ((= click-count 1) (evil-visual-char))
           ((= click-count 2) (evil-visual-line))
           ((= click-count 3) (evil-visual-block))))

        (evil-visual-pre-command)
        (unless (memq (car-safe event) '(switch-frame select-window))
          ;; Automatic hscrolling did not occur during the call to
          ;; `read-event'; but if the user subsequently drags the
          ;; mouse, go ahead and hscroll.
          (let ((auto-hscroll-mode auto-hscroll-mode-saved))
            (redisplay))
          (setq end (event-end event)
                end-point (posn-point end))
          (if (and (eq (posn-window end) start-window)
                   (integer-or-marker-p end-point))
              (evil-mouse--drag-set-mark-and-point start-point
                                                   end-point click-count)
            (let ((mouse-row (cdr (cdr (mouse-position)))))
              (cond
               ((null mouse-row))
               ((< mouse-row top)
                (mouse-scroll-subr start-window (- mouse-row top)
                                   nil start-point))
               ((>= mouse-row bottom)
                (mouse-scroll-subr start-window (1+ (- mouse-row bottom))
                                   nil start-point))))))
        (evil-visual-post-command)))

    ;; Store the fact that the current region was set by the mouse
    (setq evil--region-from-mouse t)

    ;; Handle the terminating event if possible.
    (when (consp event)
      ;; Ensure that point is on the end of the last event.
      (when (and (setq end-point (posn-point (event-end event)))
                 (eq (posn-window end) start-window)
                 (integer-or-marker-p end-point)
                 (/= start-point end-point))
        (evil-mouse--drag-set-mark-and-point start-point
                                             end-point click-count))

      ;; Find its binding.
      (let* ((fun (key-binding (vector (car event))))
             (do-multi-click (and (> (event-click-count event) 0)
                                  (functionp fun)
                                  (not (memq fun '(mouse-set-point
                                                   mouse-set-region))))))
        (if (and (or (/= (mark) (point))
                     (= click-count 1) ; word selection
                     (and (memq (evil-visual-type) '(line block))))
                 (not do-multi-click))

            ;; If point has moved, finish the drag.
            (let (last-command this-command)
              (and mouse-drag-copy-region
                   do-mouse-drag-region-post-process
                   (let (deactivate-mark)
                     (evil-visual-expand-region)
                     (copy-region-as-kill (mark) (point))
                     (evil-visual-contract-region))))

          ;; If point hasn't moved, run the binding of the
          ;; terminating up-event.
          (if do-multi-click
              (goto-char start-point)
            (deactivate-mark))
          (when (and (functionp fun)
                     (= start-hscroll (window-hscroll start-window))
                     ;; Don't run the up-event handler if the window
                     ;; start changed in a redisplay after the
                     ;; mouse-set-point for the down-mouse event at
                     ;; the beginning of this function.  When the
                     ;; window start has changed, the up-mouse event
                     ;; contains a different position due to the new
                     ;; window contents, and point is set again.
                     (or end-point
                         (= (window-start start-window)
                            start-window-start)))
            (when (and on-link
                       (= start-point (point))
                       (evil-mouse--remap-link-click-p start-event event))
              ;; If we rebind to mouse-2, reselect previous selected
              ;; window, so that the mouse-2 event runs in the same
              ;; situation as if user had clicked it directly.  Fixes
              ;; the bug reported by juri@jurta.org on 2005-12-27.
              (if (or (vectorp on-link) (stringp on-link))
                  (setq event (aref on-link 0))
                (select-window original-window)
                (setcar event 'mouse-2)
                ;; If this mouse click has never been done by the
                ;; user, it doesn't have the necessary property to be
                ;; interpreted correctly.
                (put 'mouse-2 'event-kind 'mouse-click)))
            (push event unread-command-events)))))))

;; This function is a plain copy of `mouse--drag-set-mark-and-point',
;; which is only available in Emacs 24
(defun evil-mouse--drag-set-mark-and-point (start click click-count)
  (let* ((range (evil-mouse-start-end start click click-count))
         (beg (nth 0 range))
         (end (nth 1 range)))
    (cond ((eq (mark) beg)
           (goto-char end))
          ((eq (mark) end)
           (goto-char beg))
          ((< click (mark))
           (set-mark end)
           (goto-char beg))
          (t
           (set-mark beg)
           (goto-char end)))))

;; This function is a plain copy of `mouse--remap-link-click-p',
;; which is only available in Emacs 23
(defun evil-mouse--remap-link-click-p (start-event end-event)
  (or (and (eq mouse-1-click-follows-link 'double)
           (= (event-click-count start-event) 2))
      (and
       (not (eq mouse-1-click-follows-link 'double))
       (= (event-click-count start-event) 1)
       (= (event-click-count end-event) 1)
       (or (not (integerp mouse-1-click-follows-link))
           (let ((t0 (posn-timestamp (event-start start-event)))
                 (t1 (posn-timestamp (event-end   end-event))))
             (and (integerp t0) (integerp t1)
                  (if (> mouse-1-click-follows-link 0)
                      (<= (- t1 t0) mouse-1-click-follows-link)
                    (< (- t0 t1) mouse-1-click-follows-link))))))))

(defun evil-mouse-start-end (start end mode)
  "Return a list of region bounds based on START and END according to MODE.
If MODE is not 1 then set point to (min START END), mark to (max
START END).  If MODE is 1 then set point to start of word at (min
START END), mark to end of word at (max START END)."
  (evil-sort start end)
  (setq mode (mod mode 4))
  (if (/= mode 1) (list start end)
    (list
     (save-excursion
       (goto-char (min (point-max) (1+ start)))
       (if (zerop (forward-thing evil-mouse-word -1))
           (let ((bpnt (point)))
             (forward-thing evil-mouse-word +1)
             (if (> (point) start) bpnt (point)))
         (point-min)))
     (save-excursion
       (goto-char end)
       (1-
        (if (zerop (forward-thing evil-mouse-word +1))
            (let ((epnt (point)))
              (forward-thing evil-mouse-word -1)
              (if (<= (point) end) epnt (point)))
          (point-max)))))))

;;; State switching

(evil-define-command evil-exit-emacs-state (&optional buffer message)
  "Exit Emacs state.
Changes the state to the previous state, or to Normal state
if the previous state was Emacs state."
  :keep-visual t
  :suppress-operator t
  (interactive '(nil t))
  (with-current-buffer (or buffer (current-buffer))
    (when (evil-emacs-state-p)
      (evil-change-to-previous-state buffer message)
      (when (evil-emacs-state-p)
        (evil-normal-state (and message 1))))))

(evil-define-local-var evil--execute-normal-eol-pos nil
  "Vim has special behaviour for executing in normal state at eol.
This var stores the eol position, so it can be restored when necessary.")

(defun evil--restore-repeat-hooks ()
  "No insert-state repeat info is recorded after executing in normal state.
Restore the disabled repeat hooks on insert-state exit."
  (evil-repeat-stop)
  (add-hook 'pre-command-hook 'evil-repeat-pre-hook)
  (add-hook 'post-command-hook 'evil-repeat-post-hook)
  (remove-hook 'evil-insert-state-exit-hook 'evil--restore-repeat-hooks))

(defvar evil--execute-normal-return-state nil
  "The state to return to after executing in normal state.")

(defun evil-execute-in-normal-state ()
  "Execute the next command in Normal state."
  (interactive)
  (evil-delay '(not (memq this-command
                          '(nil
                            evil-execute-in-normal-state
                            evil-replace-state
                            evil-use-register
                            digit-argument
                            negative-argument
                            universal-argument
                            universal-argument-minus
                            universal-argument-more
                            universal-argument-other-key)))
      `(progn
         (with-current-buffer ,(current-buffer)
           (when (and evil--execute-normal-eol-pos
                      (= (point) (1- evil--execute-normal-eol-pos))
                      (not (memq this-command '(evil-insert
                                                evil-goto-mark))))
             (forward-char))
           (unless (memq evil-state '(replace insert))
             (evil-change-state ',evil-state))
           (when (eq 'insert evil-state)
             (remove-hook 'pre-command-hook 'evil-repeat-pre-hook)
             (remove-hook 'post-command-hook 'evil-repeat-post-hook)
             (add-hook 'evil-insert-state-exit-hook 'evil--restore-repeat-hooks))
           (setq evil-move-cursor-back ',evil-move-cursor-back
                 evil-move-beyond-eol ',evil-move-beyond-eol
                 evil-execute-normal-keys nil)))
    'post-command-hook)
  (setq evil-insert-count nil
        evil--execute-normal-return-state evil-state
        evil--execute-normal-eol-pos (when (eolp) (point))
        evil-move-cursor-back nil
        evil-execute-normal-keys (this-command-keys))
  (evil-normal-state)
  (setq evil-move-beyond-eol t)
  (evil-echo "Switched to Normal state for the next command ..."))

(defun evil-stop-execute-in-emacs-state ()
  (when (and (not (eq this-command #'evil-execute-in-emacs-state))
             (not (minibufferp)))
    (remove-hook 'post-command-hook 'evil-stop-execute-in-emacs-state)
    (when (buffer-live-p evil-execute-in-emacs-state-buffer)
      (with-current-buffer evil-execute-in-emacs-state-buffer
        (if (and (eq evil-previous-state 'visual)
                 (not (use-region-p)))
            (progn
              (evil-change-to-previous-state)
              (evil-exit-visual-state))
          (evil-change-to-previous-state))))
    (setq evil-execute-in-emacs-state-buffer nil)))

(evil-define-command evil-execute-in-emacs-state ()
  "Execute the next command in Emacs state."
  (add-hook 'post-command-hook #'evil-stop-execute-in-emacs-state t)
  (setq evil-execute-in-emacs-state-buffer (current-buffer))
  (cond
   ((evil-visual-state-p)
    (let ((mrk (mark))
          (pnt (point)))
      (evil-emacs-state)
      (set-mark mrk)
      (goto-char pnt)))
   (t
    (evil-emacs-state)))
  (evil-echo "Switched to Emacs state for the next command ..."))

(defun evil-exit-visual-and-repeat (event)
  "Exit insert state and repeat event.
This special command should be used if some command called from
visual state should actually be called in normal-state.  The main
reason for doing this is that the repeat system should *not*
record the visual state information for some command.  This
command should be bound to exactly the same event in visual state
as the original command is bound in normal state.  EVENT is the
event that triggered the execution of this command."
  (interactive "e")
  (when (evil-visual-state-p)
    (evil-exit-visual-state)
    (push event unread-command-events)))
(evil-declare-ignore-repeat 'evil-exit-visual-and-repeat)

(provide 'evil-commands)

;;; evil-commands.el ends here
