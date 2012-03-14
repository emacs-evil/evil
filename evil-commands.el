;;;; Commands

(require 'evil-common)
(require 'evil-digraphs)
(require 'evil-search)
(require 'evil-ex)
(require 'evil-types)

;;; Motions

;; Movement commands, or motions, are defined with the macro
;; `evil-define-motion'. A motion is a command with an optional
;; argument COUNT (accessed with the special interactive code "<c>").
;; It may specify the :type command property (e.g., :type line),
;; which determines how it is handled by an operator command.
;; Furthermore, the command must have the command properties
;; :keep-visual t and :repeat motion; these are automatically
;; handled by the `evil-define-motion' macro.

(evil-define-motion evil-forward-char (count)
  "Move cursor to the right by COUNT characters."
  :type exclusive
  ;; restrict movement to the current line
  (evil-narrow-to-line-if (not evil-cross-lines)
    (evil-motion-loop (nil (or count 1))
      (forward-char)
      ;; don't put the cursor on a newline
      (when (and evil-cross-lines evil-move-cursor-back)
        (unless (or (evil-visual-state-p) (evil-operator-state-p))
          (when (and (eolp) (not (eobp)) (not (bolp)))
            (forward-char)))))))

(evil-define-motion evil-backward-char (count)
  "Move cursor to the left by COUNT characters."
  :type exclusive
  ;; restrict movement to the current line
  (evil-narrow-to-line-if (not evil-cross-lines)
    (evil-motion-loop (nil (or count 1))
      (backward-char)
      ;; don't put the cursor on a newline
      (unless (or (evil-visual-state-p) (evil-operator-state-p))
        (evil-adjust-cursor)))))

(evil-define-motion evil-next-line (count)
  "Move the cursor COUNT lines down."
  :type line
  (let (line-move-visual)
    (evil-line-move (or count 1))
    (evil-adjust-cursor)))

(evil-define-motion evil-previous-line (count)
  "Move the cursor COUNT lines up."
  :type line
  (let (line-move-visual)
    (evil-line-move (- (or count 1)))
    (evil-adjust-cursor)))

(evil-define-motion evil-next-visual-line (count)
  "Move the cursor COUNT screen lines down."
  :type exclusive
  (let ((line-move-visual t))
    (evil-line-move (or count 1))
    (evil-adjust-cursor)))

(evil-define-motion evil-previous-visual-line (count)
  "Move the cursor COUNT screen lines up."
  :type exclusive
  (let ((line-move-visual t))
    (evil-line-move (- (or count 1)))
    (evil-adjust-cursor)))

;; used for repeated commands like "dd"
(evil-define-motion evil-line (count)
  "Move COUNT - 1 lines down."
  :type line
  (let (line-move-visual)
    (evil-line-move (1- (or count 1)))
    ;; select the previous line at the end of the buffer
    (evil-adjust-cursor)))

(evil-define-motion evil-beginning-of-line ()
  "Move the cursor to the beginning of the current line."
  :type exclusive
  (move-beginning-of-line nil))

(evil-define-motion evil-end-of-line (count)
  "Move the cursor to the end of the current line.
If COUNT is given, move COUNT - 1 lines downward first."
  :type inclusive
  (move-end-of-line count)
  (unless (evil-visual-state-p)
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
    (end-of-line count))
  (unless (evil-visual-state-p)
    (evil-adjust-cursor)))

(evil-define-motion evil-beginning-of-line-or-digit-argument ()
  "Move the cursor to the beginning of the current line.
This function passes its command to `digit-argument' (usually a 0)
if it is not the first event."
  :type exclusive
  (cond
   (current-prefix-arg
    (setq this-command #'digit-argument)
    (call-interactively #'digit-argument))
   (t
    (setq this-command #'evil-beginning-of-line)
    (call-interactively #'evil-beginning-of-line))))

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
  (evil-next-line (or count 1))
  (evil-first-non-blank))

(evil-define-motion evil-next-line-1-first-non-blank (count)
  "Move the cursor COUNT-1 lines down on the first non-blank character."
  :type line
  (evil-next-line (1- (or count 1)))
  (evil-first-non-blank))

(evil-define-motion evil-previous-line-first-non-blank (count)
  "Move the cursor COUNT lines up on the first non-blank character."
  :type line
  (evil-previous-line (or count 1))
  (evil-first-non-blank))

(evil-define-motion evil-goto-line (count)
  "Go to the first non-blank character of line COUNT.
By default the last line."
  :jump t
  :type line
  (if (null count)
      (goto-char (point-max))
    (goto-char (point-min))
    (forward-line (1- count)))
  (evil-adjust-cursor)
  (evil-first-non-blank))

(evil-define-motion evil-goto-first-line (count)
  "Go to the first non-blank character of line COUNT.
By default the first line."
  :jump t
  :type line
  (evil-goto-line (or count 1)))

(evil-define-motion evil-move-empty-lines (count)
  "Move to the next or previous empty line, repeated COUNT times."
  :type exclusive
  (evil-motion-loop (var (or count 1))
    (cond
     ((< var 0)
      (goto-char
       (or (save-excursion
             (unless (bobp)
               (backward-char)
               (re-search-backward "^$" nil t)))
           (point))))
     (t
      (let ((orig (point)))
        (when (re-search-forward "^$" nil t)
          (if (eobp)
              (goto-char orig)
            (forward-char))))))))

(evil-define-union-move evil-move-word (count)
  "Move by words."
  (evil-move-chars evil-word count)
  (evil-move-chars (evil-concat-charsets "^ \t\r\n" evil-word) count)
  (evil-move-empty-lines count))

(evil-define-union-move evil-move-WORD (count)
  "Move by WORDs."
  (evil-move-chars "^ \t\r\n" count)
  (evil-move-empty-lines count))

(evil-define-motion evil-forward-word-begin (count &optional bigword)
  "Move the cursor to the beginning of the COUNT-th next word.
If BIGWORD is non-nil, move by WORDS."
  :type exclusive
  (let ((move (if bigword #'evil-move-WORD #'evil-move-word))
        (orig (point)))
    (prog1 (if (and evil-want-change-word-to-end
                    (eq evil-this-operator #'evil-change))
               (evil-move-end count move)
             (evil-move-beginning count move))
      ;; if we reached the beginning of a word on a new line in
      ;; Operator-Pending state, go back to the end of the previous
      ;; line
      (when (and (evil-operator-state-p)
                 (> (line-beginning-position) orig)
                 (looking-back "^[[:space:]]*"))
        ;; move cursor back as long as the line contains only
        ;; whitespaces and is non-empty
        (evil-move-end-of-line 0)
        ;; skip non-empty lines containing only spaces
        (while (and (looking-back "^[[:space:]]+$")
                    (not (<= (line-beginning-position) orig)))
          (evil-move-end-of-line 0))
        ;; but if the previous line is empty, delete this line
        (when (bolp) (forward-char))))))

(evil-define-motion evil-forward-word-end (count &optional bigword)
  "Move the cursor to the end of the COUNT-th next word.
If BIGWORD is non-nil, move by WORDS."
  :type inclusive
  (let ((move (if bigword #'evil-move-WORD #'evil-move-word)))
    ;; if changing a one-letter word, don't move point to the
    ;; next word (which would change two words)
    (if (and (evil-operator-state-p)
             (looking-at (format "[%s]" evil-word)))
        (prog1 (evil-move-end count move)
          (unless (bobp) (backward-char)))
      (evil-move-end count move nil t))))

(evil-define-motion evil-backward-word-begin (count &optional bigword)
  "Move the cursor to the beginning of the COUNT-th previous word.
If BIGWORD is non-nil, move by WORDS."
  :type exclusive
  (let ((move (if bigword #'evil-move-WORD #'evil-move-word)))
    (evil-move-beginning (- (or count 1)) move)))

(evil-define-motion evil-backward-word-end (count &optional bigword)
  "Move the cursor to the end of the COUNT-th previous word.
If BIGWORD is non-nil, move by WORDS."
  :type inclusive
  (let ((move (if bigword #'evil-move-WORD #'evil-move-word)))
    (evil-move-end (- (or count 1)) move nil t)))

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
  (beginning-of-defun (- (or count 1))))

(evil-define-motion evil-forward-section-end (count)
  "Move the cursor to the end of the COUNT-th next section."
  :jump t
  :type inclusive
  (end-of-defun (or count 1)))

(evil-define-motion evil-backward-section-begin (count)
  "Move the cursor to the beginning of the COUNT-th previous section."
  :jump t
  :type exclusive
  (beginning-of-defun (or count 1)))

(evil-define-motion evil-backward-section-end (count)
  "Move the cursor to the end of the COUNT-th previous section."
  :jump t
  :type inclusive
  (end-of-defun (- (or count 1))))

(evil-define-motion evil-forward-sentence (count)
  "Move to the next COUNT-th beginning of a sentence or end of a paragraph."
  :jump t
  :type exclusive
  (let ((count (or count 1))
        beg-sentence end-paragraph)
    (when (evil-eobp)
      (signal 'end-of-buffer nil))
    (evil-motion-loop (nil count)
      (unless (eobp)
        (setq beg-sentence
              (save-excursion
                (and (zerop (evil-move-beginning 1 #'evil-move-sentence))
                     (point)))
              end-paragraph
              (save-excursion
                (forward-paragraph)
                (point)))
        (evil-goto-min beg-sentence end-paragraph)))))

(evil-define-motion evil-backward-sentence (count)
  "Move to the previous COUNT-th beginning of a sentence or paragraph."
  :jump t
  :type exclusive
  (let ((count (or count 1))
        beg-sentence beg-paragraph)
    (when (bobp)
      (signal 'beginning-of-buffer nil))
    (evil-motion-loop (nil count)
      (unless (bobp)
        (setq beg-sentence
              (save-excursion
                (and (zerop (evil-move-beginning -1 #'evil-move-sentence))
                     (point)))
              beg-paragraph
              (save-excursion
                (backward-paragraph)
                (point)))
        (evil-goto-max beg-sentence beg-paragraph)))))

(evil-define-motion evil-forward-paragraph (count)
  "Move to the end of the COUNT-th next paragraph."
  :jump t
  :type exclusive
  (evil-move-end count #'forward-paragraph #'backward-paragraph))

(evil-define-motion evil-backward-paragraph (count)
  "Move to the beginning of the COUNT-th previous paragraph."
  :jump t
  :type exclusive
  (evil-move-beginning (- (or count 1))
                       #'forward-paragraph #'backward-paragraph))

;; TODO: this is a very basic implementation considering only
;; (), [], {}, and not blocks like #if ... #endif
(evil-define-motion evil-jump-item (count)
  "Find the next item in this line after or under the cursor
and jump to the corresponding one."
  :jump t
  :type inclusive
  (cond
   ;; COUNT% jumps to a line COUNT percentage down the file
   (count
    (goto-char
     (evil-normalize-position
      (let ((size (- (point-max) (point-min))))
        (+ (point-min)
           (if (> size 80000)
               (* count (/ size 100))
             (/ (* count size) 100))))))
    (back-to-indentation)
    (setq evil-this-type 'line))
   (t
    (let* ((next-open
            (condition-case err
                (1- (scan-lists (point) 1 -1))
              (error
               (point-max))))
           (next-close
            (condition-case nil
                (1- (scan-lists (point) 1 1))
              (error (point-max))))
           (pos (min next-open next-close)))
      (cond
       ((>= pos (line-end-position))
        (error "No matching item found on the current line"))
       ((= pos next-open)
        (goto-char pos)
        (forward-list)
        (backward-char))
       (t
        (goto-char (1+ pos))
        (backward-list)))))))

(evil-define-motion evil-previous-open-paren (count)
  "Go to [count] previous unmatched '('."
  :type exclusive
  (let ((range (save-excursion
                 (backward-char)
                 (evil-paren-range count ?\( ?\)))))
    (when range
      (goto-char (evil-range-beginning range)))))

(evil-define-motion evil-next-close-paren (count)
  "Go to [count] next unmatched ')'."
  :type exclusive
  (let ((range (save-excursion
                 (forward-char)
                 (evil-paren-range count ?\( ?\)))))
    (when range
      (goto-char (1- (evil-range-end range))))))

(evil-define-motion evil-previous-open-brace (count)
  "Go to [count] previous unmatched '{'."
  :type exclusive
  (let ((range (save-excursion
                 (backward-char)
                 (evil-paren-range count ?\{ ?\}))))
    (when range
      (goto-char (evil-range-beginning range)))))

(evil-define-motion evil-next-close-brace (count)
  "Go to [count] next unmatched '}'."
  :type exclusive
  (let ((range (save-excursion
                 (forward-char)
                 (evil-paren-range count ?\{ ?\}))))
    (when range
      (goto-char (1- (evil-range-end range))))))

(evil-define-motion evil-find-char (count char)
  "Move to the next COUNT'th occurrence of CHAR."
  :jump t
  :type inclusive
  (interactive "<c>c")
  (setq count (or count 1))
  (let ((fwd (> count 0)))
    (setq evil-last-find (list #'evil-find-char char fwd))
    (when fwd (forward-char))
    (let ((case-fold-search nil))
      (unless (prog1
                  (search-forward (char-to-string char)
                                  (unless evil-cross-lines
                                    (if fwd
                                        (line-end-position)
                                      (line-beginning-position)))
                                  t count)
                (when fwd (backward-char)))
        (error "Can't find %c" char)))))

(evil-define-motion evil-find-char-backward (count char)
  "Move to the previous COUNT'th occurrence of CHAR."
  :jump t
  :type exclusive
  (interactive "<c>c")
  (evil-find-char (- (or count 1)) char))

(evil-define-motion evil-find-char-to (count char)
  "Move before the next COUNT'th occurrence of CHAR."
  :jump t
  :type inclusive
  (interactive "<c>c")
  (unwind-protect
      (progn
        (evil-find-char count char)
        (if (> (or count 1) 0)
            (backward-char)
          (forward-char)))
    (setcar evil-last-find #'evil-find-char-to)))

(evil-define-motion evil-find-char-to-backward (count char)
  "Move before the previous COUNT'th occurrence of CHAR."
  :jump t
  :type exclusive
  (interactive "<c>c")
  (evil-find-char-to (- (or count 1)) char))

(evil-define-motion evil-repeat-find-char (count)
  "Repeat the last find COUNT times."
  :jump t
  :type inclusive
  (setq count (or count 1))
  (if evil-last-find
      (let ((cmd (car evil-last-find))
            (char (nth 1 evil-last-find))
            (fwd (nth 2 evil-last-find))
            evil-last-find)
        (funcall cmd (if fwd count (- count)) char)
        (unless (nth 2 evil-last-find)
          (setq evil-this-type 'exclusive)))
    (error "No previous search")))

(evil-define-motion evil-repeat-find-char-reverse (count)
  "Repeat the last find COUNT times in the opposite direction."
  :jump t
  :type inclusive
  (evil-repeat-find-char (- (or count 1))))

;; ceci n'est pas une pipe
(evil-define-motion evil-goto-column (count)
  "Go to column COUNT on the current line.
Columns are counted from zero."
  :type exclusive
  (move-to-column (or count 0)))

(evil-define-command evil-goto-mark (char)
  "Go to the marker specified by CHAR."
  :keep-visual t
  :repeat nil
  :type exclusive
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
     (t
      (error "Marker `%c' is not set%s" char
             (if (evil-global-marker-p char) ""
               " in this buffer"))))))

(evil-define-command evil-goto-mark-line (char)
  "Go to the line of the marker specified by CHAR."
  :keep-visual t
  :repeat nil
  :type line
  (interactive (list (read-char)))
  (evil-goto-mark char)
  (evil-first-non-blank))

(evil-define-motion evil-jump-backward (count)
  "Go to older position in jump list.
To go the other way, press \
\\<evil-motion-state-map>\\[evil-jump-forward]."
  (let ((current-pos (make-marker))
        (count (or count 1)) i)
    (unless evil-jump-list
      (move-marker current-pos (point))
      (add-to-list 'evil-jump-list current-pos))
    (evil-motion-loop (nil count)
      (setq current-pos (make-marker))
      ;; skip past duplicate entries in the mark ring
      (setq i (length mark-ring))
      (while (progn (move-marker current-pos (point))
                    (set-mark-command 0)
                    (setq i (1- i))
                    (and (= (point) current-pos) (> i 0))))
      ;; Already there?
      (move-marker current-pos (point))
      (unless (= current-pos (car-safe evil-jump-list))
        (add-to-list 'evil-jump-list current-pos)))))

(evil-define-motion evil-jump-forward (count)
  "Go to newer position in jump list.
To go the other way, press \
\\<evil-motion-state-map>\\[evil-jump-backward]."
  (let ((count (or count 1))
        current-pos next-pos)
    (evil-motion-loop (nil count)
      (setq current-pos (car-safe evil-jump-list)
            next-pos (car (cdr-safe evil-jump-list)))
      (when next-pos
        (push-mark current-pos t nil)
        (unless (eq (marker-buffer next-pos) (current-buffer))
          (switch-to-buffer (marker-buffer next-pos)))
        (goto-char next-pos)
        (pop evil-jump-list)))))

(evil-define-motion evil-jump-to-tag (arg)
  "Jump to tag under point.
If called with a prefix argument, provide a prompt
for specifying the tag."
  :jump t
  (interactive "P")
  (if arg (call-interactively #'find-tag)
    (let ((tag (thing-at-point 'symbol)))
      (find-tag tag))))

(evil-define-motion evil-lookup ()
  "Look up the keyword at point.
Calls `evil-lookup-func'."
  (funcall evil-lookup-func))

(evil-define-motion evil-ret (count)
  "Move the cursor COUNT lines down.
If point is on a widget or a button, click on it.
In Insert state, insert a newline."
  :type line
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
      (if (not evil-auto-indent)
          (newline count)
        (delete-horizontal-space t)
        (newline count)
        (indent-according-to-mode)))
     (t
      (evil-next-line count)))))

(evil-define-motion evil-window-top (count)
  "Move the cursor to line COUNT from the top of the window
on the first non-blank character."
  :jump t
  :type line
  (move-to-window-line (or count 0))
  (back-to-indentation))

(evil-define-motion evil-window-middle ()
  "Move the cursor to the middle line in the window
on the first non-blank character."
  :jump t
  :type line
  (move-to-window-line
   (/ (1+ (save-excursion (move-to-window-line -1))) 2))
  (back-to-indentation))

(evil-define-motion evil-window-bottom (count)
  "Move the cursor to line COUNT from the bottom of the window
on the first non-blank character."
  :jump t
  :type line
  (move-to-window-line (- (or count 1)))
  (back-to-indentation))

;; scrolling
(evil-define-command evil-scroll-line-up (count)
  "Scrolls the window COUNT lines upwards."
  :repeat nil
  :keep-visual t
  (interactive "p")
  (scroll-down count))

(evil-define-command evil-scroll-line-down (count)
  "Scrolls the window COUNT lines downwards."
  :repeat nil
  :keep-visual t
  (interactive "p")
  (scroll-up count))

(evil-define-command evil-scroll-up (count)
  "Scrolls the window and the cursor COUNT lines upwards.
The default is half the screen."
  :repeat nil
  :keep-visual t
  (interactive "P")
  (evil-save-column
    (let ((p (point))
          (c (or count (/ (evil-num-visible-lines) 2))))
      (save-excursion
        (scroll-down (min (evil-max-scroll-up) c)))
      (forward-line (- c))
      (when (= (line-number-at-pos p)
               (line-number-at-pos (point)))
        (signal 'beginning-of-buffer nil)))))

(evil-define-command evil-scroll-down (count)
  "Scrolls the window and the cursor COUNT lines downwards.
The default is half the screen."
  :repeat nil
  :keep-visual t
  (interactive "P")
  (evil-save-column
    (let ((p (point))
          (c (or count (/ (evil-num-visible-lines) 2))))
      (save-excursion
        (scroll-up (min (evil-max-scroll-down) c)))
      (forward-line c)
      (when (= (line-number-at-pos p)
               (line-number-at-pos (point)))
        (signal 'end-of-buffer nil)))))

(evil-define-command evil-scroll-page-up (count)
  "Scrolls the window COUNT pages upwards."
  :repeat nil
  :keep-visual t
  (interactive "p")
  (evil-save-column
    (dotimes (i count)
      (scroll-down nil))))

(evil-define-command evil-scroll-page-down (count)
  "Scrolls the window COUNT pages upwards."
  :repeat nil
  :keep-visual t
  (interactive "p")
  (evil-save-column
    (dotimes (i count)
      (scroll-up nil))))

(evil-define-command evil-scroll-line-to-top (count)
  "Scrolls line number COUNT (or the cursor line) to the top of the window."
  :repeat nil
  :keep-visual t
  (interactive "P")
  (evil-save-column
    (let ((line (or count (line-number-at-pos (point)))))
      (goto-char (point-min))
      (forward-line (1- line)))
    (recenter 0)))

(evil-define-command evil-scroll-line-to-center (count)
  "Scrolls line number COUNT (or the cursor line) to the center of the window."
  :repeat nil
  :keep-visual t
  (interactive "P")
  (evil-save-column
    (when count
      (goto-char (point-min))
      (forward-line (1- count)))
    (recenter nil)))

(evil-define-command evil-scroll-line-to-bottom (count)
  "Scrolls line number COUNT (or the cursor line) to the bottom of the window."
  :repeat nil
  :keep-visual t
  (interactive "P")
  (evil-save-column
    (let ((line (or count (line-number-at-pos (point)))))
      (goto-char (point-min))
      (forward-line (1- line)))
    (recenter -1)))

(evil-define-command evil-scroll-bottom-line-to-top (count)
  "Scrolls the line right below the window,
or line COUNT to the top of the window."
  :repeat nil
  :keep-visual t
  (interactive "P")
  (if count
      (progn
        (goto-char (point-min))
        (forward-line (1- count)))
    (goto-char (window-end))
    (evil-move-cursor-back))
  (recenter 0)
  (evil-first-non-blank))

(evil-define-command evil-scroll-top-line-to-bottom (count)
  "Scrolls the line right below the window,
or line COUNT to the top of the window."
  :repeat nil
  :keep-visual t
  (interactive "P")
  (if count
      (progn
        (goto-char (point-min))
        (forward-line (1- count)))
    (goto-char (window-start)))
  (recenter -1)
  (evil-first-non-blank))

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

(evil-define-text-object evil-a-word (count &optional bigword)
  "Select a word.
If BIGWORD is non-nil, select a WORD."
  (evil-an-object-range count (if bigword
                                  #'evil-move-WORD
                                #'evil-move-word)))

(evil-define-text-object evil-inner-word (count &optional bigword)
  "Select inner word.
If BIGWORD is non-nil, select inner WORD."
  (evil-inner-object-range count (if bigword
                                     #'evil-move-WORD
                                   #'evil-move-word)))

(evil-define-text-object evil-a-WORD (count)
  "Select a WORD."
  (evil-a-word count t))

(evil-define-text-object evil-inner-WORD (count)
  "Select inner WORD."
  (evil-inner-word count t))

(evil-define-text-object evil-a-sentence (count)
  "Select a sentence."
  (evil-an-object-range count #'evil-move-sentence nil nil t))

(evil-define-text-object evil-inner-sentence (count)
  "Select inner sentence."
  (evil-inner-object-range count #'evil-move-sentence))

(evil-define-text-object evil-a-paragraph (count)
  "Select a paragraph."
  :type line
  (evil-an-object-range count #'evil-move-paragraph nil nil t))

(evil-define-text-object evil-inner-paragraph (count)
  "Select inner paragraph."
  :type line
  (evil-inner-object-range count #'evil-move-paragraph))

(evil-define-text-object evil-a-paren (count)
  "Select a parenthesis."
  :extend-selection nil
  (evil-paren-range count ?\( ?\)))

(evil-define-text-object evil-inner-paren (count)
  "Select inner parenthesis."
  :extend-selection nil
  (evil-paren-range count ?\( ?\) t))

(evil-define-text-object evil-a-bracket (count)
  "Select a square bracket."
  :extend-selection nil
  (evil-paren-range count ?\[ ?\]))

(evil-define-text-object evil-inner-bracket (count)
  "Select inner square bracket."
  :extend-selection nil
  (evil-paren-range count ?\[ ?\] t))

(evil-define-text-object evil-a-curly (count)
  "Select a curly bracket (\"brace\")."
  :extend-selection nil
  (evil-paren-range count ?{ ?}))

(evil-define-text-object evil-inner-curly (count)
  "Select inner curly bracket (\"brace\")."
  :extend-selection nil
  (evil-paren-range count ?{ ?} t))

(evil-define-text-object evil-an-angle (count)
  "Select an angle bracket."
  :extend-selection nil
  (evil-paren-range count ?< ?>))

(evil-define-text-object evil-inner-angle (count)
  "Select inner angle bracket."
  :extend-selection nil
  (evil-paren-range count ?< ?> t))

(evil-define-text-object evil-a-single-quote (count)
  "Select a single-quoted expression."
  :extend-selection t
  (evil-quote-range count ?' ?'))

(evil-define-text-object evil-inner-single-quote (count)
  "Select inner single-quoted expression."
  :extend-selection nil
  ;; TODO: do not use `current-prefix-arg' (requires better handling
  ;; in visual-state code of `evil-define-text-object')!
  (evil-quote-range current-prefix-arg ?' ?' t))

(evil-define-text-object evil-a-double-quote (count)
  "Select a double-quoted expression."
  :extend-selection t
  (evil-quote-range count ?\" ?\"))

(evil-define-text-object evil-inner-double-quote (count)
  "Select inner double-quoted expression."
  :extend-selection nil
  (evil-quote-range current-prefix-arg ?\" ?\" t))

(evil-define-text-object evil-a-back-quote (count)
  "Select a back-quoted expression."
  :extend-selection t
  (evil-quote-range count ?\` ?\`))

(evil-define-text-object evil-inner-back-quote (count)
  "Select inner back-quoted expression."
  :extend-selection nil
  (evil-quote-range current-prefix-arg ?\` ?\` t))

(evil-define-text-object evil-a-tag (count)
  "Select a tag block."
  :extend-selection nil
  (evil-xml-range count))

(evil-define-text-object evil-inner-tag (count)
  "Select inner tag block."
  :extend-selection nil
  (cond
   ((and (evil-called-interactively-p)
         (eq last-command this-command))
    (setq this-command #'evil-a-tag)
    (evil-a-tag count))
   (t
    (evil-xml-range count t))))

;;; Operator commands

(evil-define-operator evil-yank (beg end type register yank-handler)
  "Saves the characters in motion into the kill-ring."
  :move-point nil
  :repeat nil
  (interactive "<R><x><y>")
  (cond
   ((and (fboundp 'cua--global-mark-active)
         (fboundp 'cua-copy-region-to-global-mark)
         (cua--global-mark-active))
    (cua-copy-region-to-global-mark beg end))
   ((eq type 'block)
    (evil-yank-rectangle beg end register yank-handler))
   ((eq type 'line)
    (evil-yank-lines beg end register yank-handler))
   (t
    (evil-yank-characters beg end register yank-handler))))

(evil-define-operator evil-yank-line (beg end type register)
  "Saves whole lines into the kill-ring."
  :motion evil-line
  :move-point nil
  (interactive "<R><x>")
  (evil-yank beg end type register))

(evil-define-operator evil-delete (beg end type register yank-handler)
  "Delete text from BEG to END with TYPE.
Save in REGISTER or in the kill-ring with YANK-HANDLER."
  (interactive "<R><x><y>")
  (evil-yank beg end type register yank-handler)
  (if (eq type 'block)
      (delete-rectangle beg end)
    (delete-region beg end))
  ;; place cursor on beginning of line
  (when (eq type 'line)
    (evil-first-non-blank)))

(evil-define-operator evil-delete-line (beg end type register yank-handler)
  "Delete to end of line."
  :motion nil
  :keep-visual t
  (interactive "<R><x>")
  ;; act linewise in Visual state
  (let* ((beg (or beg (point)))
         (end (or end beg)))
    (when (evil-visual-state-p)
      (unless (memq type '(line block))
        (let ((range (evil-expand beg end 'line)))
          (setq beg (evil-range-beginning range)
                end (evil-range-end range)
                type (evil-type range))))
      (evil-exit-visual-state))
    (cond
     ((eq type 'block)
      (evil-apply-on-block #'evil-delete-line
                           beg end nil register yank-handler))
     ((eq type 'line)
      (evil-delete beg end type register yank-handler))
     (t
      (evil-delete beg (line-end-position) type register yank-handler)))))

(evil-define-operator evil-delete-whole-line
  (beg end type register yank-handler)
  "Delete whole line."
  :motion evil-line
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

(evil-define-operator evil-delete-backward-word (beg end type register)
  "Delete previous word."
  :motion evil-backward-word-begin
  (interactive "<R><x>")
  (evil-delete beg end type register))

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
        (nlines (1+ (- (line-number-at-pos end)
                       (line-number-at-pos beg)))))
    (funcall delete-func beg end type register yank-handler)
    (cond
     ((eq type 'line)
      (evil-open-above 1))
     ((eq type 'block)
      (evil-insert 1 nlines))
     (t
      (evil-insert 1)))))

(evil-define-operator evil-change-line (beg end type register yank-handler)
  "Change to end of line."
  :motion evil-end-of-line
  (interactive "<R><x><y>")
  (evil-change beg end type register yank-handler #'evil-delete-line))

(evil-define-operator evil-change-whole-line
  (beg end type register yank-handler)
  "Change whole line."
  :motion evil-line
  (interactive "<R><x>")
  (evil-change beg end type register yank-handler #'evil-delete-whole-line))

(evil-define-operator evil-substitute (beg end type register)
  "Change a character."
  :motion evil-forward-char
  (interactive "<R><x>")
  (evil-change beg end type register))

(evil-define-operator evil-upcase (beg end type)
  "Convert text to upper case."
  (if (eq type 'block)
      (evil-apply-on-block #'evil-upcase beg end)
    (upcase-region beg end)))

(evil-define-operator evil-downcase (beg end type)
  "Convert text to lower case."
  (if (eq type 'block)
      (evil-apply-on-block #'evil-downcase beg end)
    (downcase-region beg end)))

(evil-define-operator evil-invert-case (beg end type)
  "Invert case of text."
  (let (char)
    (if (eq type 'block)
        (evil-apply-on-block #'evil-invert-case beg end)
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
      (evil-apply-on-block #'evil-invert-case beg end)
    (evil-invert-case beg end)
    (when evil-this-motion
      (goto-char end))))

(evil-define-operator evil-rot13 (beg end type)
  "ROT13 encrypt text."
  (if (eq type 'block)
      (evil-apply-on-block #'evil-rot13 beg end)
    (rot13-region beg end)))

(evil-define-operator evil-join (beg end)
  "Join the selected lines."
  :motion evil-line
  (let ((count (count-lines beg end)))
    (when (> count 1)
      (setq count (1- count)))
    (dotimes (var count)
      (join-line 1))))

(evil-define-operator evil-join-whitespace (beg end)
  "Join the selected lines without changing whitespace.
\\<evil-normal-state-map>Like \\[evil-join], \
but doesn't insert or remove any spaces."
  :motion evil-line
  (let ((count (count-lines beg end)))
    (when (> count 1)
      (setq count (1- count)))
    (dotimes (var count)
      (evil-move-end-of-line 1)
      (unless (eobp)
        (delete-char 1)))))

(evil-define-operator evil-fill (beg end)
  "Fill text."
  :move-point nil
  :type line
  (save-excursion
    (condition-case nil
        (fill-region beg end)
      (error nil))))

(evil-define-operator evil-indent (beg end)
  "Indent text."
  :move-point nil
  :type line
  (if (and (= beg (line-beginning-position))
           (= end (line-beginning-position 2)))
      ;; since some Emacs modes can only indent one line at a time,
      ;; implement "==" as a call to `indent-according-to-mode'
      (indent-according-to-mode)
    (goto-char beg)
    (indent-region beg end))
  (back-to-indentation))

(evil-define-operator evil-indent-line (beg end)
  "Indent the line."
  :motion evil-line
  (evil-indent beg end))

(evil-define-operator evil-shift-left (beg end)
  "Shift text from BEG to END to the left.
The text is shifted to the nearest multiple of `evil-shift-width'
\(the rounding can be disabled by setting `evil-shift-round').
See also `evil-shift-right'."
  :type line
  (if (not evil-shift-round)
      (indent-rigidly beg end (- evil-shift-width))
    (let* ((indent
            (save-excursion
              (goto-char beg)
              (evil-move-beginning-of-line)
              ;; ignore blank lines
              (while (and (< (point) end) (looking-at "[ \t]*$"))
                (forward-line))
              (if (> (point) end) 0
                (current-indentation))))
           (offset (1+ (mod (1- indent) evil-shift-width))))
      (indent-rigidly beg end (- offset)))))

(evil-define-operator evil-shift-right (beg end)
  "Shift text from BEG to END to the right.
The text is shifted to the nearest multiple of `evil-shift-width'
\(the rounding can be disabled by setting `evil-shift-round').
See also `evil-shift-left'."
  :type line
  (if (not evil-shift-round)
      (indent-rigidly beg end evil-shift-width)
    (let* ((indent
            (save-excursion
              (goto-char beg)
              (evil-move-beginning-of-line nil)
              (while (and (< (point) end) (looking-at "[ \t]*$"))
                (forward-line))
              (if (> (point) end) 0
                (current-indentation))))
           (offset (- evil-shift-width (mod indent evil-shift-width))))
      (indent-rigidly beg end offset))))

(evil-define-operator evil-align-left (beg end type &optional width)
  "Right-align lines in the region at WIDTH columns.
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
  "Centers lines in the region between WIDTH columns.
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
               (evil-save-cursor
                 (evil-refresh-cursor 'replace)
                 (list (evil-read-key))))
  (when char
    (if (eq type 'block)
        (save-excursion
          (evil-apply-on-block #'evil-replace beg end nil char))
      (goto-char beg)
      (while (< (point) end)
        (if (eq (char-after) ?\n)
            (forward-char)
          (delete-char 1)
          (insert-char char 1)))
      (if (eq char ?\n)
          (when evil-auto-indent
            (indent-according-to-mode))
        (goto-char (max beg (1- end)))))))

(evil-define-command evil-paste-before
  (count &optional register yank-handler)
  "Pastes the latest yanked text before the cursor position.
The return value is the yanked text."
  :suppress-operator t
  (interactive "P<x>")
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
                    (this-command #'evil-paste-before))
                (push-mark opoint t)
                (insert-for-yank text))
            ;; no yank-handler, default
            (set-text-properties 0 (length text) nil text)
            (push-mark opoint t)
            (dotimes (i (or count 1))
              (insert-for-yank text))
            (setq evil-last-paste
                  (list #'evil-paste-before
                        count
                        opoint
                        opoint    ; beg
                        (point))) ; end
            (evil-set-marker ?\[ opoint)
            (evil-set-marker ?\] (1- (point)))
            (when (> (length text) 0)
              (backward-char))))
        ;; no paste-pop after pasting from a register
        (when register
          (setq evil-last-paste nil))
        (and (> (length text) 0) text)))))

(evil-define-command evil-paste-after
  (count &optional register yank-handler)
  "Pastes the latest yanked text behind point.
The return value is the yanked text."
  :suppress-operator t
  (interactive "P<x>")
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
            (set-text-properties 0 (length text) nil text)
            (unless (eolp) (forward-char))
            (push-mark (point) t)
            ;; TODO: Perhaps it is better to collect a list of all
            ;; (point . mark) pairs to undo the yanking for COUNT > 1.
            ;; The reason is that this yanking could very well use
            ;; `yank-handler'.
            (let ((beg (point)))
              (dotimes (i (or count 1))
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
        (when register
          (setq evil-last-paste nil))
        (and (> (length text) 0) text)))))

(evil-define-command evil-visual-paste (count &optional register)
  "Paste over Visual selection."
  :suppress-operator t
  (interactive "P<x>")
  (let* ((text (if register
                   (evil-get-register register)
                 (current-kill 0)))
         (yank-handler (car-safe (get-text-property
                                  0 'yank-handler text))))
    (evil-with-undo
      (when (evil-visual-state-p)
        ;; add replaced text to the kill-ring
        (unless register
          ;; if pasting from the kill-ring,
          ;; add replaced text before the current kill
          (setq kill-ring (delete text kill-ring)))
        (setq kill-ring-yank-pointer kill-ring)
        (evil-visual-rotate 'upper-left)
        (evil-delete evil-visual-beginning evil-visual-end
                     (evil-visual-type))
        (unless register
          (kill-new text))
        (when (and (eq yank-handler #'evil-yank-line-handler)
                   (not (eq (evil-visual-type) 'line)))
          (insert "\n"))
        (evil-normal-state))
      (evil-paste-before count register))))

(defun evil-paste-from-register (register)
  "Paste from REGISTER."
  (interactive
   (let ((overlay (make-overlay (point) (point)))
         (string "\""))
     (unwind-protect
         (progn
           ;; display " in the buffer while reading register
           (put-text-property 0 1 'face 'minibuffer-prompt string)
           (put-text-property 0 1 'cursor t string)
           (overlay-put overlay 'after-string string)
           (list (or evil-this-register (read-char))))
       (delete-overlay overlay))))
  (when (fboundp 'evil-paste-before)
    (when (evil-paste-before nil register t)
      ;; go to end of pasted text
      (forward-char))))

(evil-define-command evil-use-register (register)
  "Use REGISTER for the next command."
  :keep-visual t
  (interactive "c")
  (setq evil-this-register register))

(evil-define-command evil-record-macro (register)
  "Record a keyboard macro into REGISTER."
  :keep-visual t
  :suppress-operator t
  (interactive
   (list (unless evil-this-macro
           (or evil-this-register (read-char)))))
  (cond
   (evil-this-macro
    (condition-case nil
        (end-kbd-macro)
      (error nil))
    (when last-kbd-macro
      (when (member last-kbd-macro '("" []))
        (setq last-kbd-macro nil))
      (evil-set-register evil-this-macro last-kbd-macro))
    (setq evil-this-macro nil))
   (t
    (setq evil-this-macro register)
    (evil-set-register evil-this-macro nil)
    (start-kbd-macro nil))))

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
     (if (eq register ?@)
         (setq macro last-kbd-macro)
       (setq macro (evil-get-register register t)))
     (list count macro)))
  (if (or (and (not (stringp macro))
               (not (vectorp macro)))
          (member macro '("" [])))
      ;; allow references to currently empty registers
      ;; when defining macro
      (unless evil-this-macro
        (error "No previous macro"))
    (condition-case nil
        (execute-kbd-macro macro count)
      ;; enter Normal state if the macro fails
      (error (evil-normal-state)
             (evil-normalize-keymaps)))))

;;; Visual commands

(evil-define-motion evil-visual-restore ()
  "Restore previous selection."
  (let* ((point (point))
         (mark (or (mark t) point))
         (dir evil-visual-direction)
         (type (evil-visual-type))
         range)
    (unless (evil-visual-state-p)
      (cond
       ;; No previous selection.
       ((or (null evil-visual-selection)
            (null evil-visual-mark)
            (null evil-visual-point)))
       ;; If the type was one-to-one, it is preferable to infer
       ;; point and mark from the selection's boundaries. The reason
       ;; is that a destructive operation may displace the markers
       ;; inside the selection.
       ((evil-type-property type :one-to-one)
        (setq range (evil-contract-range (evil-visual-range))
              mark (evil-range-beginning range)
              point (evil-range-end range))
        (when (< dir 0)
          (evil-swap mark point)))
       ;; If the type wasn't one-to-one, we have to restore the
       ;; selection on the basis of the previous point and mark.
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
the next VCOUNT-1 lines starting at the same column.
If SKIP-EMPTY-LINES is non-nil, the insertion will not be performed
on lines on which the insertion point would be after the end of the
lines. This is the default behaviour for Visual-state insertion."
  (interactive
   (list (prefix-numeric-value current-prefix-arg)
         (when (evil-visual-state-p)
           (evil-visual-rotate 'upper-left)
           (when (memq (evil-visual-type) '(line block))
             (count-lines evil-visual-beginning
                          evil-visual-end)))
         (evil-visual-state-p)))
  (if (and (evil-called-interactively-p)
           (evil-visual-state-p)
           (and (eq (evil-visual-type) 'line)))
      (evil-insert-line count vcount)
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
the next VCOUNT-1 lines starting at the same column. If
SKIP-EMPTY-LINES is non-nil, the insertion will not be performed
on lines on which the insertion point would be after the end of
the lines."
  (interactive
   (list (prefix-numeric-value current-prefix-arg)
         (when (evil-visual-state-p)
           (evil-visual-rotate (if (eq (evil-visual-type) 'block)
                                   'upper-right
                                 'upper-left))
           (when (memq (evil-visual-type) '(line block))
             (save-excursion
               ;; go to upper-left corner temporarily so
               ;; `count-lines' yields accurate results
               (evil-visual-rotate 'upper-left)
               (count-lines evil-visual-beginning
                            evil-visual-end))))))
  (if (and (evil-called-interactively-p)
           (evil-visual-state-p)
           (and (eq (evil-visual-type) 'line)))
      (evil-append-line count vcount)
    (unless (or (eolp) (evil-visual-state-p))
      (forward-char))
    (evil-insert count vcount skip-empty-lines)))

(defun evil-insert-resume (count)
  "Switch to Insert state at previous insertion point."
  (interactive "p")
  (when (evil-get-marker ?^)
    (goto-char (evil-get-marker ?^)))
  (evil-insert count))

(defun evil-maybe-remove-spaces ()
  "Remove space from newly opened empty line.
This function should be called from `post-command-hook' after
`evil-open-above' or `evil-open-below'. If the last command
finished insert state and if the current line consists of
whitespaces only, then those spaces have been inserted because of
the indentation. In this case those spaces are removed leaving a
completely empty line."
  (unless (memq this-command '(evil-open-above evil-open-below))
    (remove-hook 'post-command-hook 'evil-maybe-remove-spaces)
    (when (and (not (evil-insert-state-p))
               (save-excursion
                 (beginning-of-line)
                 (looking-at "^\\s-*$")))
      (delete-region (line-beginning-position)
                     (line-end-position)))))

(defun evil-open-above (count)
  "Insert a new line above point and switch to Insert state.
The insertion will be repeated COUNT times."
  (interactive "p")
  (evil-insert-newline-above)
  (setq evil-insert-count count
        evil-insert-lines t
        evil-insert-vcount nil)
  (when evil-auto-indent
    (indent-according-to-mode))
  (evil-insert-state 1)
  (add-hook 'post-command-hook #'evil-maybe-remove-spaces))

(defun evil-open-below (count)
  "Insert a new line below point and switch to Insert state.
The insertion will be repeated COUNT times."
  (interactive "p")
  (evil-insert-newline-below)
  (setq evil-insert-count count
        evil-insert-lines t
        evil-insert-vcount nil)
  (when evil-auto-indent
    (indent-according-to-mode))
  (evil-insert-state 1)
  (add-hook 'post-command-hook #'evil-maybe-remove-spaces))

(defun evil-insert-line (count &optional vcount)
  "Switch to Insert state just before the first non-blank character
on the current line. The insertion will be repeated COUNT times."
  (interactive "p")
  (if evil-auto-indent
      (back-to-indentation)
    (evil-move-beginning-of-line))
  (setq evil-insert-count count
        evil-insert-lines nil
        evil-insert-vcount
        (and vcount
             (> vcount 1)
             (list (line-number-at-pos)
                   #'evil-first-non-blank
                   vcount)))
  (evil-insert-state 1))

(defun evil-append-line (count &optional vcount)
  "Switch to Insert state at the end of the current line.
The insertion will be repeated COUNT times."
  (interactive "p")
  (evil-move-end-of-line)
  (setq evil-insert-count count
        evil-insert-lines nil
        evil-insert-vcount
        (and vcount
             (> vcount 1)
             (list (line-number-at-pos)
                   #'end-of-line
                   vcount)))
  (evil-insert-state 1))

(defun evil-insert-digraph (count digraph)
  "Insert the digraph DIGRAPH.
The insertion is repeated COUNT times."
  (interactive
   (let (count char1 char2 overlay string)
     (unwind-protect
         (progn
           (setq count (prefix-numeric-value current-prefix-arg)
                 overlay (make-overlay (point) (point)))
           ;; create overlay prompt
           (setq string "?")
           (put-text-property 0 1 'face 'minibuffer-prompt string)
           ;; put cursor at (i.e., right before) the prompt
           (put-text-property 0 1 'cursor t string)
           (overlay-put overlay 'after-string string)
           (setq char1 (evil-read-key))
           (setq string (string char1))
           (put-text-property 0 1 'face 'minibuffer-prompt string)
           (put-text-property 0 1 'cursor t string)
           (overlay-put overlay 'after-string string)
           (setq char2 (evil-read-key)))
       (delete-overlay overlay))
     (list count (list char1 char2))))
  (let ((digraph (or (evil-digraph digraph)
                     ;; use the last character if undefined
                     (cadr digraph))))
    (dotimes (var count)
      (insert digraph))))

(defun evil-copy-from-above (arg)
  "Copy characters from preceding non-blank line.
The copied text is inserted before point.
ARG is the number of lines to move backward."
  (interactive
   (cond
    ;; if a prefix argument was given, repeat it for subsequent calls
    ((and (null current-prefix-arg)
          (eq last-command #'evil-copy-from-above))
     (setq current-prefix-arg last-prefix-arg)
     (list (prefix-numeric-value current-prefix-arg)))
    (t
     (list (prefix-numeric-value current-prefix-arg)))))
  (insert (evil-copy-chars-from-line 1 (- arg))))

(defun evil-copy-from-below (arg)
  "Copy characters from following non-blank line.
The copied text is inserted before point.
ARG is the number of lines to move forward."
  (interactive
   (cond
    ((and (null current-prefix-arg)
          (eq last-command #'evil-copy-from-below))
     (setq current-prefix-arg last-prefix-arg)
     (list (prefix-numeric-value current-prefix-arg)))
    (t
     (list (prefix-numeric-value current-prefix-arg)))))
  (insert (evil-copy-chars-from-line 1 arg)))

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
  "Called to record a search command."
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
  (dotimes (var (or count 1))
    (evil-search (if evil-regexp-search
                     (car-safe regexp-search-ring)
                   (car-safe search-ring))
                 isearch-forward evil-regexp-search)))

(evil-define-motion evil-search-previous (count)
  "Repeat the last search in the opposite direction."
  :jump t
  :type exclusive
  (dotimes (var (or count 1))
    (evil-search (if evil-regexp-search
                     (car-safe regexp-search-ring)
                   (car-safe search-ring))
                 (not isearch-forward) evil-regexp-search)))

(evil-define-motion evil-search-symbol-backward (count)
  "Search backward for symbol under point."
  :jump t
  :type exclusive
  (dotimes (var (or count 1))
    (evil-search-symbol nil)))

(evil-define-motion evil-search-symbol-forward (count)
  "Search forward for symbol under point."
  :jump t
  :type exclusive
  (dotimes (var (or count 1))
    (evil-search-symbol t)))

(evil-define-motion evil-goto-definition ()
  "Go to definition or first occurrence of symbol under point."
  :jump t
  :type exclusive
  (let* ((string (evil-find-symbol t))
         (search (format "\\_<%s\\_>" (regexp-quote string)))
         ientry ipos)
    ;; load imenu if available
    (unless (featurep 'imenu)
      (condition-case nil
          (require 'imenu)
        (error nil)))
    (if (null string)
        (error "No symbol under cursor")
      (setq isearch-forward t)
      ;; if imenu is available, try it
      (cond
       ((fboundp 'imenu--make-index-alist)
        (condition-case nil
            (setq ientry (imenu--make-index-alist))
          (error nil))
        (setq ientry (assoc string ientry))
        (setq ipos (cdr ientry))
        (when (and (markerp ipos)
                   (eq (marker-buffer ipos) (current-buffer)))
          (setq ipos (marker-position ipos)))
        (cond
         ;; imenu found a position, so go there and
         ;; highlight the occurrence
         ((numberp ipos)
          (evil-search search t t ipos))
         ;; imenu failed, so just go to first occurrence in buffer
         (t
          (evil-search search t t (point-min)))))
       ;; no imenu, so just go to first occurrence in buffer
       (t
        (evil-search search t t (point-min)))))))

;;; Folding

(evil-define-command evil-toggle-fold ()
  "Open or close a fold."
  (when (fboundp 'hs-minor-mode)
    (hs-minor-mode 1))
  (when (fboundp 'hs-toggle-hiding)
    (hs-toggle-hiding)))

(evil-define-command evil-open-folds ()
  "Open all folds.
See also `evil-close-folds'."
  (when (fboundp 'hs-minor-mode)
    (hs-minor-mode 1))
  (when (fboundp 'hs-show-all)
    (hs-show-all)))

(evil-define-command evil-close-folds ()
  "Close all folds.
See also `evil-open-folds'."
  (when (fboundp 'hs-minor-mode)
    (hs-minor-mode 1))
  (when (fboundp 'hs-hide-all)
    (hs-hide-all)))

(evil-define-command evil-open-fold ()
  "Open fold.
See also `evil-close-fold'."
  (when (fboundp 'hs-minor-mode)
    (hs-minor-mode 1))
  (when (fboundp 'hs-show-block)
    (hs-show-block)))

(evil-define-command evil-close-fold ()
  "Close fold.
See also `evil-open-fold'."
  (when (fboundp 'hs-minor-mode)
    (hs-minor-mode 1))
  (when (fboundp 'hs-hide-block)
    (hs-hide-block)))

;;; Ex

(evil-define-operator evil-write (beg end type filename &optional bang)
  "Save the current buffer, from BEG to END, to FILENAME.
If the file already exists and the BANG argument is non-nil,
it is overwritten without confirmation."
  :motion nil
  :move-point nil
  :type line
  :repeat nil
  (interactive "<R><f><!>")
  (when (zerop (length filename))
    (setq filename (buffer-file-name)))
  (cond
   ((zerop (length filename))
    (error "Please specify a file name for the buffer"))
   ((and beg end)
    (write-region beg end filename nil nil nil (not bang)))
   ((and (not bang) (string= filename (or (buffer-file-name) "")))
    (save-buffer))
   (t
    (write-file filename (not bang)))))

(evil-define-command evil-write-all (bang)
  "Saves all buffers."
  :repeat nil
  :move-point nil
  (interactive "<!>")
  (save-some-buffers bang))

(evil-define-command evil-save (file &optional bang)
  "Save the current buffer to FILE.
Changes the file name of the current buffer to this name.
If no FILE is given, the current file name is used."
  :repeat nil
  :move-point nil
  (interactive "<f><!>")
  (evil-write nil nil nil file bang))

(evil-define-command evil-edit (file &optional bang)
  "Open FILE.
If no FILE is specified, reload the current buffer from disk."
  :repeat nil
  (interactive "<f><!>")
  (if file
      (find-file file)
    (revert-buffer bang (or bang (not (buffer-modified-p))) t)))

(evil-define-command evil-read (count file)
  "Inserts the contents of FILE below the current line or line COUNT."
  :repeat nil
  :move-point nil
  (interactive "P<fsh>")
  (when (and file (not (zerop (length file))))
    (when count (goto-char (point-min)))
    (when (or (not (zerop (forward-line (or count 1))))
              (not (bolp)))
      (insert "\n"))
    (if (/= (aref file 0) ?!)
        (let ((result (insert-file-contents file)))
          (save-excursion
            (forward-char (cadr result))
            (unless (bolp) (insert "\n"))))
      (shell-command (substring file 1) t)
      (save-excursion
        (goto-char (mark))
        (unless (bolp) (insert "\n"))))))

(evil-define-command evil-show-buffers ()
  "Shows the buffer-list."
  :repeat nil
  (let (message-truncate-lines message-log-max)
    (display-message-or-buffer
     (mapconcat 'identity
                (sort
                 (mapcar #'buffer-name (buffer-list))
                 'string<)
                "\n")
     "*Buffers*")))

(evil-define-command evil-buffer (buffer)
  "Switches to another buffer."
  :repeat nil
  (interactive "<b>")
  (if buffer
      (when (or (get-buffer buffer)
                (y-or-n-p (format "No buffer with name \"%s\" exists. \
Create new buffer? " buffer)))
        (switch-to-buffer buffer))
    (switch-to-buffer (other-buffer))))

(evil-define-command evil-next-buffer (&optional count)
  "Goes to the `count'-th next buffer in the buffer list."
  :repeat nil
  (interactive "p")
  (dotimes (i (or count 1))
    (next-buffer)))

(evil-define-command evil-prev-buffer (&optional count)
  "Goes to the `count'-th prev buffer in the buffer list."
  :repeat nil
  (interactive "p")
  (dotimes (i (or count 1))
    (previous-buffer)))

(evil-define-command evil-delete-buffer (buffer &optional bang)
  "Deletes a buffer."
  (interactive "<b><!>")
  (with-current-buffer (or buffer (current-buffer))
    (when bang
      (set-buffer-modified-p nil))
    ;; if the buffer which was initiated by emacsclient,
    ;; call `server-edit' from server.el to avoid
    ;; "Buffer still has clients" message
    (if (and (fboundp 'server-edit)
             (boundp 'server-buffer-clients)
             server-buffer-clients)
        (server-edit)
      (kill-buffer nil))))

(evil-define-command evil-quit (&optional bang)
  "Closes the current window, exits Emacs if this is the last window."
  :repeat nil
  (interactive "<!>")
  (condition-case nil
      (delete-window)
    (error
     (condition-case nil
         (delete-frame)
       (error
        (if (null bang)
            (save-buffers-kill-emacs)
          (dolist (process (process-list))
            (set-process-query-on-exit-flag process nil))
          (kill-emacs)))))))

(evil-define-command evil-quit-all (&optional bang)
  "Exits Emacs, asking for saving."
  :repeat nil
  (interactive "<!>")
  (if (null bang)
      (save-buffers-kill-emacs)
    (dolist (process (process-list))
      (set-process-query-on-exit-flag process nil))
    (kill-emacs)))

(evil-define-command evil-save-and-quit ()
  "Exits Emacs, without saving."
  (save-buffers-kill-emacs 1))

(evil-define-command evil-save-and-close (file &optional bang)
  "Saves the current buffer and closes the window."
  :repeat nil
  (interactive "<f><!>")
  (evil-write (point-min) (point-max) 'line file bang)
  (evil-quit))

(evil-define-command evil-save-modified-and-close (file &optional bang)
  "Saves the current buffer and closes the window."
  :repeat nil
  (interactive "<f><!>")
  (when (buffer-modified-p)
    (evil-write (point-min) (point-max) 'line file bang))
  (evil-quit))

(evil-define-operator evil-shell-command
  (beg end command &optional previous)
  "Execute a shell command.
If BEG and END is specified, COMMAND is executed on the region,
which is replaced with the command's output. Otherwise, the
output is displayed in its own buffer. If PREVIOUS is non-nil,
the previous shell command is executed instead."
  :motion nil
  (interactive "<r><sh><!>")
  (when command
    (setq command (evil-ex-replace-special-filenames command)))
  (if (zerop (length command))
      (when previous (setq command evil-previous-shell-command))
    (setq evil-previous-shell-command command))
  (cond
   ((zerop (length command))
    (if previous (error "No previous shell command")
      (error "No shell command")))
   ((and beg end)
    (shell-command-on-region beg end command t)
    (goto-char beg)
    (evil-first-non-blank))
   (t
    (shell-command command))))

;; TODO: escape special characters (currently only \n) ... perhaps
;; there is some Emacs function doing this?
(evil-define-command evil-show-registers ()
  "Shows the contents of all registers."
  :repeat nil
  (let (message-truncate-lines message-log-max)
    (message "%s"
             (mapconcat #'(lambda (reg)
                            (format "\"%c\t%s"
                                    (car reg)
                                    (if (stringp (cdr reg))
                                        (replace-regexp-in-string "\n" "^J" (cdr reg))
                                      (cdr reg))))
                        (evil-register-list) "\n"))))

(eval-when-compile (require 'ffap))
(evil-define-command evil-find-file-at-point-with-line ()
  "Opens the file at point and goes to line-number."
  (let ((fname (ffap-file-at-point)))
    (if fname
        (let ((line
               (save-excursion
                 (goto-char (cadr ffap-string-at-point-region))
                 (and (re-search-backward ":\\([0-9]+\\)\\="
                                          (line-beginning-position) t)
                      (string-to-number (match-string 1))))))
          (ffap-other-window)
          (when line
            (goto-char (point-min))
            (forward-line (1- line))))
      (error "File does not exist."))))

(evil-ex-define-argument-type state (flag &rest args)
  "Defines an argument type which can take state names."
  (when (eq flag 'complete)
    (let ((arg (pop args))
          (predicate (pop args))
          (flag (pop args))
          (completions
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
          (test-completion arg completions predicate)))))))

;; TODO: should we merge this command with `evil-set-initial-state'?
(evil-define-command evil-ex-set-initial-state (state)
  "Set the initial state for the current major mode to STATE.
This is the state the buffer comes up in. See `evil-set-initial-state'."
  :ex-arg state
  :repeat nil
  (interactive "<sym>")
  (if (not (or (assq state evil-state-properties)
               (null state)))
      (error "State %s cannot be set as initial Evil state" state)
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
  "Goes to the next occurrence."
  :jump t
  :type exclusive
  (setq evil-ex-search-start-point (point))
  (let ((orig (point))
        wrapped)
    (dotimes (i (or count 1))
      (if (eq evil-ex-search-direction 'backward)
          (backward-char)
        (forward-char))
      (let ((res (evil-ex-find-next)))
        (cond
         ((not res)
          (goto-char orig)
          (signal 'search-failed
                  (list (evil-ex-pattern-regex evil-ex-search-pattern))))
         ((eq res 'wrapped) (setq wrapped t)))))
    (if wrapped
        (let (message-log-max)
          (message "Search wrapped")))
    (goto-char (match-beginning 0))
    (setq evil-ex-search-match-beg (match-beginning 0)
          evil-ex-search-match-end (match-end 0))
    (evil-ex-search-goto-offset evil-ex-search-offset)
    (evil-ex-search-activate-highlight evil-ex-search-pattern)))

(evil-define-motion evil-ex-search-previous (count)
  "Goes the the previous occurrence."
  :jump t
  :type exclusive
  (let ((evil-ex-search-direction
         (if (eq evil-ex-search-direction 'backward) 'forward 'backward)))
    (evil-ex-search-next count)))

(defun evil-repeat-ex-search (flag)
  "Called to record a search command."
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
    (evil-repeat-record (evil-ex-pattern-regex evil-ex-search-pattern)))
   (t (evil-repeat-motion flag))))

(evil-define-motion evil-ex-search-forward (count)
  "Starts a forward search."
  :jump t
  :type exclusive
  :repeat evil-repeat-ex-search
  (evil-ex-start-search 'forward count))

(evil-define-motion evil-ex-search-backward (count)
  "Starts a forward search."
  :jump t
  :repeat evil-repeat-ex-search
  (evil-ex-start-search 'backward count))

(evil-define-motion evil-ex-search-symbol-forward (count)
  "Search for the next occurrence of word under the cursor."
  :jump t
  :type exclusive
  (evil-ex-start-symbol-search nil 'forward count))

(evil-define-motion evil-ex-search-symbol-backward (count)
  "Search for the next occurrence of word under the cursor."
  :jump t
  :type exclusive
  (evil-ex-start-symbol-search nil 'backward count))

(evil-define-motion evil-ex-search-unbounded-symbol-forward (count)
  "Search for the next occurrence of word under the cursor."
  :jump t
  :type exclusive
  (evil-ex-start-symbol-search t 'forward count))

(evil-define-motion evil-ex-search-unbounded-symbol-backward (count)
  "Search for the next occurrence of word under the cursor."
  :jump t
  :type exclusive
  (evil-ex-start-symbol-search t 'backward count))

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
    (error "No pattern given"))
  (unless replacement
    (error "No replacement given"))
  (let* ((flags (append flags nil))
         (confirm (memq ?c flags))
         (case-replace (eq (evil-ex-pattern-case-fold pattern) 'insensitive))
         (case-fold-search case-replace)
         (evil-ex-substitute-regex (evil-ex-pattern-regex pattern)))
    (setq evil-ex-substitute-pattern pattern
          evil-ex-substitute-replacement replacement
          evil-ex-substitute-flags flags)
    (if (evil-ex-pattern-whole-line pattern)
        ;; this one is easy, just use the built-in function
        (perform-replace evil-ex-substitute-regex
                         evil-ex-substitute-replacement
                         confirm t nil nil nil beg end)
      (let ((evil-ex-substitute-nreplaced 0)
            (evil-ex-substitute-next-line (line-number-at-pos beg))
            (evil-ex-substitute-last-line
             (if (save-excursion (goto-char end) (bolp))
                 (1- (line-number-at-pos end))
               (line-number-at-pos end)))
            (evil-ex-substitute-last-point (point)))
        (if confirm
            (let ((evil-ex-substitute-overlay
                   (make-overlay (point) (point)))
                  (evil-ex-substitute-hl
                   (evil-ex-make-hl 'evil-ex-substitute)))
              (evil-ex-hl-change 'evil-ex-substitute pattern)
              (unwind-protect
                  ;; this one is more difficult: we have to do
                  ;; the highlighting and querying on our own
                  (progn
                    (overlay-put evil-ex-substitute-overlay
                                 'face 'isearch)
                    (overlay-put evil-ex-substitute-overlay
                                 'priority 1001)
                    (map-y-or-n-p
                     #'(lambda (x)
                         (set-match-data x)
                         (move-overlay evil-ex-substitute-overlay
                                       (match-beginning 0)
                                       (match-end 0))
                         (format "Query replacing %s with %s: "
                                 (match-string 0)
                                 (evil-match-substitute-replacement
                                  evil-ex-substitute-replacement
                                  (not case-replace))))
                     #'(lambda (x)
                         (set-match-data x)
                         (evil-replace-match evil-ex-substitute-replacement
                                             (not case-replace))
                         (setq evil-ex-substitute-last-point (point))
                         (setq evil-ex-substitute-nreplaced
                               (1+ evil-ex-substitute-nreplaced))
                         (evil-ex-hl-set-region 'evil-ex-substitute
                                                (save-excursion
                                                  (forward-line)
                                                  (point))
                                                (evil-ex-hl-get-max
                                                 'evil-ex-substitute)))
                     #'(lambda ()
                         (goto-char (point-min))
                         (when (and
                                (zerop
                                 (forward-line
                                  (1- evil-ex-substitute-next-line)))
                                (bolp)
                                (re-search-forward
                                 evil-ex-substitute-regex
                                 nil t nil)
                                (<= (line-number-at-pos (match-end 0))
                                    evil-ex-substitute-last-line))
                           (goto-char (match-beginning 0))
                           (setq evil-ex-substitute-next-line
                                 (1+ (line-number-at-pos (point))))
                           (match-data)))))
                (evil-ex-delete-hl 'evil-ex-substitute)
                (delete-overlay evil-ex-substitute-overlay)))

          ;; just replace the first occurrences per line
          ;; without highlighting and asking
          (goto-char (point-min))
          (let ((num (1- evil-ex-substitute-next-line)))
            (while (and (zerop (forward-line num))
                        (bolp)
                        (re-search-forward
                         evil-ex-substitute-regex nil t nil)
                        (<= (line-number-at-pos
                             (match-beginning 0))
                            evil-ex-substitute-last-line))
              (setq evil-ex-substitute-nreplaced
                    (1+ evil-ex-substitute-nreplaced))
              (evil-replace-match evil-ex-substitute-replacement
                                  (not case-replace))
              (setq evil-ex-substitute-last-point (point))
              (setq num 1))))

        (goto-char evil-ex-substitute-last-point)

        (message "Replaced %d occurrence%s"
                 evil-ex-substitute-nreplaced
                 (if (/= evil-ex-substitute-nreplaced 1) "s" ""))))
    (evil-first-non-blank)))

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

(evil-define-operator evil-ex-global
  (beg end pattern command &optional invert)
  "The Ex global command.
\[BEG,END]global[!]/PATTERN/COMMAND"
  :motion mark-whole-buffer
  :move-point nil
  (interactive "<r><g/><!>")
  (unless pattern
    (error "No pattern given"))
  (unless command
    (error "No command given"))
  (let ((case-fold-search
         (eq (evil-ex-regex-case pattern 'smart) 'insensitive))
        match markers)
    (when (and pattern command)
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
            (evil-ex-eval command))
        ;; ensure that all markers are deleted afterwards,
        ;; even in the event of failure
        (dolist (marker markers)
          (set-marker marker nil))))))

(evil-define-operator evil-ex-global-inverted
  (beg end pattern command &optional invert)
  "The Ex vglobal command.
\[BEG,END]vglobal/PATTERN/COMMAND"
  :motion mark-whole-buffer
  :move-point nil
  (interactive "<r><g/><!>")
  (evil-ex-global beg end pattern command (not invert)))

(evil-define-command evil-goto-char (position)
  "Go to POSITION in the buffer.
Default position is the beginning of the buffer."
  (interactive "p")
  (let ((position (evil-normalize-position
                   (or position (point-min)))))
    (goto-char position)))

(evil-define-operator evil-ex-line-number (beg end)
  "Print the last line number."
  :motion mark-whole-buffer
  :move-point nil
  (interactive "<r>")
  (message "%d" (count-lines (point-min) end)))

(evil-define-command evil-show-file-info ()
  "Shows basic file information."
  (let* ((nlines   (count-lines (point-min) (point-max)))
         (curr     (line-number-at-pos (point)))
         (perc     (* (/ (float curr) (float nlines)) 100.0))
         (file     (buffer-file-name))
         (writable (and file (file-writable-p file)))
         (readonly (if (and file (not writable)) "[readonly] " "")))
    (if file
        (message "\"%s\" %d %slines --%d%%--" file nlines readonly perc)
      (message "%d lines --%d%%--" nlines perc))))

;;; Window navigation

(defun evil-resize-window (new-size &optional horizontal)
  "Sets the current window's with or height to `new-size'."
  (let ((wincfg (current-window-configuration))
        (nwins (length (window-list)))
        (count (if horizontal
                   (- new-size (window-width))
                 (- new-size (window-height)))))
    (catch 'done
      (save-window-excursion
        (while (not (zerop count))
          (if (> count 0)
              (progn
                (enlarge-window 1 horizontal)
                (setq count (1- count)))
            (progn
              (shrink-window 1 horizontal)
              (setq count (1+ count))))
          (if (= nwins (length (window-list)))
              (setq wincfg (current-window-configuration))
            (throw 'done t)))))
    (set-window-configuration wincfg)))

(defun evil-get-buffer-tree (wintree)
  "Extracts the buffer tree from a given window-tree."
  (if (consp wintree)
      (cons (car wintree) (mapcar #'evil-get-buffer-tree (cddr wintree)))
    (window-buffer wintree)))

(defun evil-restore-window-tree (win tree)
  "Restores the given buffer-tree layout as subwindows of win."
  (cond
   ((and (consp tree) (cddr tree))
    (let ((newwin (split-window win nil (not (car tree)))))
      (evil-restore-window-tree win (cadr tree))
      (evil-restore-window-tree newwin (cons (car tree) (cddr tree)))))
   ((consp tree)
    (set-window-buffer win (cadr tree)))
   (t
    (set-window-buffer win tree))))

(evil-define-command evil-window-split (&optional count file)
  "Splits the current window horizontally, COUNT lines height,
editing a certain FILE."
  :repeat nil
  (interactive "P<f>")
  (let ((new-win (split-window (selected-window) count)))
    (when file
      (evil-edit file))))

(evil-define-command evil-window-vsplit (&optional count file)
  "Splits the current window vertically, COUNT columns width,
editing a certain FILE."
  :repeat nil
  (interactive "P<f>")
  (let ((new-win (split-window (selected-window) count t)))
    (when file
      (evil-edit file))))

(evil-define-command evil-split-buffer (buffer)
  "Splits window and switches to another buffer."
  :repeat nil
  (interactive "<b>")
  (evil-window-split)
  (evil-buffer buffer))

(evil-define-command evil-split-next-buffer (&optional count)
  "Splits the window and goes to the COUNT-th next buffer in the buffer list."
  :repeat nil
  (interactive "p")
  (evil-window-split)
  (evil-next-buffer count))

(evil-define-command evil-split-prev-buffer (&optional count)
  "Splits window and goes to the COUNT-th prev buffer in the buffer list."
  :repeat nil
  (interactive "p")
  (evil-window-split)
  (evil-prev-buffer count))

(evil-define-command evil-window-left (count)
  "Move the cursor to new COUNT-th window left of the current one."
  :repeat nil
  (interactive "p")
  (dotimes (i count)
    (windmove-left)))

(evil-define-command evil-window-right (count)
  "Move the cursor to new COUNT-th window right of the current one."
  :repeat nil
  (interactive "p")
  (dotimes (i count)
    (windmove-right)))

(evil-define-command evil-window-up (count)
  "Move the cursor to new COUNT-th window above the current one."
  :repeat nil
  (interactive "p")
  (dotimes (i (or count 1))
    (windmove-up)))

(evil-define-command evil-window-down (count)
  "Move the cursor to new COUNT-th window below the current one."
  :repeat nil
  (interactive "p")
  (dotimes (i (or count 1))
    (windmove-down)))

(evil-define-command evil-window-bottom-right ()
  "Move the cursor to bottom-right window."
  :repeat nil
  (while (let (success)
           (condition-case nil
               (progn
                 (windmove-right)
                 (setq success t))
             (error nil))
           (condition-case nil
               (progn
                 (windmove-down)
                 (setq success t))
             (error nil))
           success)))

(evil-define-command evil-window-top-left ()
  "Move the cursor to top-left window."
  :repeat nil
  (while (let (success)
           (condition-case nil
               (progn
                 (windmove-left)
                 (setq success t))
             (error nil))
           (condition-case nil
               (progn
                 (windmove-up)
                 (setq success t))
             (error nil))
           success)))

(evil-define-command evil-window-mru ()
  "Move the cursor to the previous (last accessed) buffer in another window.
More precisely, it selectes the most recently used buffer that is
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
  (interactive "P")
  (if (not count)
      (select-window (next-window))
    (evil-window-top-left)
    (other-window (1- count))))

(evil-define-command evil-window-prev (count)
  "Move the cursor to the previous window in the cyclic order.
With COUNT go to the count-th window in the order starting from
top-left."
  :repeat nil
  (interactive "P")
  (if (not count)
      (select-window (previous-window))
    (evil-window-top-left)
    (other-window (1- count))))

(evil-define-command evil-window-new (count file)
  "Splits the current window horizontally
and opens a new buffer or edits a certain FILE."
  :repeat nil
  (interactive "P<f>")
  (split-window (selected-window) count)
  (if file
      (evil-edit file)
    (let ((buffer (generate-new-buffer "*new*")))
      (set-window-buffer (selected-window) buffer)
      (with-current-buffer buffer
        (evil-normal-state)))))

(evil-define-command evil-window-vnew (count file)
  "Splits the current window vertically
and opens a new buffer name or edits a certain FILE."
  :repeat nil
  (interactive "P<f>")
  (split-window (selected-window) count t)
  (if file
      (evil-edit file)
    (let ((buffer (generate-new-buffer "*new*")))
      (set-window-buffer (selected-window) buffer)
      (with-current-buffer buffer
        (evil-normal-state)))))

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
  "Sets the height of the current window to COUNT."
  :repeat nil
  (interactive "P")
  (evil-resize-window (or count (frame-height)) nil))

(evil-define-command evil-window-set-width (count)
  "Sets the width of the current window to COUNT."
  :repeat nil
  (interactive "P")
  (evil-resize-window (or count (frame-width)) t))

(evil-define-command evil-window-rotate-upwards ()
  "Rotates the windows according to the currenty cyclic ordering."
  :repeat nil
  (let ((wlist (window-list))
        (blist (mapcar #'(lambda (w) (window-buffer w))
                       (window-list))))
    (setq blist (append (cdr blist) (list (car blist))))
    (while (and wlist blist)
      (set-window-buffer (car wlist) (car blist))
      (setq wlist (cdr wlist)
            blist (cdr blist)))
    (select-window (car (last (window-list))))))

(evil-define-command evil-window-rotate-downwards ()
  "Rotates the windows according to the currenty cyclic ordering."
  :repeat nil
  (let ((wlist (window-list))
        (blist (mapcar #'(lambda (w) (window-buffer w))
                       (window-list))))
    (setq blist (append (last blist) blist))
    (while (and wlist blist)
      (set-window-buffer (car wlist) (car blist))
      (setq wlist (cdr wlist)
            blist (cdr blist)))
    (select-window (cadr (window-list)))))

(evil-define-command evil-window-move-very-top ()
  "Closes the current window, splits the upper-left one horizontally
and redisplays the current buffer there."
  :repeat nil
  (unless (one-window-p)
    (let ((b (current-buffer)))
      (delete-window)
      (let ((btree (evil-get-buffer-tree (car (window-tree)))))
        (delete-other-windows)
        (let ((newwin (selected-window))
              (subwin (split-window)))
          (evil-restore-window-tree subwin btree)
          (set-window-buffer newwin b)
          (select-window newwin))))
    (balance-windows)))

(evil-define-command evil-window-move-far-left ()
  "Closes the current window, splits the upper-left one vertically
and redisplays the current buffer there."
  :repeat nil
  (unless (one-window-p)
    (let ((b (current-buffer)))
      (delete-window)
      (let ((btree (evil-get-buffer-tree (car (window-tree)))))
        (delete-other-windows)
        (let ((newwin (selected-window))
              (subwin (split-window-horizontally)))
          (evil-restore-window-tree subwin btree)
          (set-window-buffer newwin b)
          (select-window newwin))))
    (balance-windows)))

(evil-define-command evil-window-move-far-right ()
  "Closes the current window, splits the lower-right one vertically
and redisplays the current buffer there."
  :repeat nil
  (unless (one-window-p)
    (let ((b (current-buffer)))
      (delete-window)
      (let ((btree (evil-get-buffer-tree (car (window-tree)))))
        (delete-other-windows)
        (let ((subwin (selected-window))
              (newwin (split-window-horizontally)))
          (evil-restore-window-tree subwin btree)
          (set-window-buffer newwin b)
          (select-window newwin))))
    (balance-windows)))

(evil-define-command evil-window-move-very-bottom ()
  "Closes the current window, splits the lower-right one horizontally
and redisplays the current buffer there."
  :repeat nil
  (unless (one-window-p)
    (let ((b (current-buffer)))
      (delete-window)
      (let ((btree (evil-get-buffer-tree (car (window-tree)))))
        (delete-other-windows)
        (let ((subwin (selected-window))
              (newwin (split-window)))
          (evil-restore-window-tree subwin btree)
          (set-window-buffer newwin b)
          (select-window newwin))))
    (balance-windows)))

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

(defun evil-execute-in-normal-state ()
  "Execute the next command in Normal state."
  (interactive)
  (evil-delay '(not (eq this-command #'evil-execute-in-normal-state))
      `(progn
         (evil-change-to-previous-state)
         (setq evil-move-cursor-back ',evil-move-cursor-back))
    'post-command-hook)
  (setq evil-move-cursor-back nil)
  (evil-normal-state)
  (evil-echo "Switched to Normal state for the next command ..."))

(defun evil-execute-in-emacs-state (&optional arg)
  "Execute the next command in Emacs state."
  (interactive "p")
  (cond
   (arg
    (add-hook 'post-command-hook #'evil-execute-in-emacs-state t)
    (evil-emacs-state)
    (evil-echo "Switched to Emacs state for the next command ..."))
   ((not (eq this-command #'evil-execute-in-emacs-state))
    (remove-hook 'post-command-hook 'evil-execute-in-emacs-state)
    (evil-change-to-previous-state))))

;; TODO: this will probably not work well with the repeat-system.
(evil-define-command evil-esc (arg)
  "Wait for further keys within `evil-esc-delay'.
Otherwise send [escape]."
  :repeat ignore
  (interactive "P")
  (if (sit-for evil-esc-delay t)
      (progn
        (push 'escape unread-command-events)
        (when defining-kbd-macro
          ;; we need to replace the ESC by 'escape in the currently
          ;; defined keyboard macro
          (evil-save-echo-area
            (end-kbd-macro)
            (setq last-kbd-macro (vconcat last-kbd-macro [escape]))
            (start-kbd-macro t t))))
    (push last-command-event unread-command-events)
    ;; preserve prefix argument
    (setq prefix-arg arg))
  ;; disable interception for the next key sequence
  (evil-esc-mode -1)
  (setq this-command last-command)
  (add-hook 'pre-command-hook #'evil-turn-on-esc-mode nil t))

(provide 'evil-commands)

;;; evil-commands.el ends here
