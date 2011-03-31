;;;; Motions

(require 'evil-vars)
(require 'evil-common)
(require 'evil-states)
(require 'evil-types)
(require 'evil-compatibility)

(evil-define-state motion
  "Motion state"
  :tag " <M> ")

(defmacro evil-define-motion (motion args &rest body)
  "Define an motion command MOTION.
ARGS is the argument list, which if non-nil must contain
the count as the first argument."
  (declare (indent defun)
           (debug (&define name lambda-list
                           [&optional stringp]
                           [&rest keywordp sexp]
                           def-body)))
  (let (arg interactive key keys type)
    (when args
      (setq args `(&optional ,@(delq '&optional args))
            interactive
            ;; the count is either numerical or nil
            '(list (when current-prefix-arg
                     (prefix-numeric-value
                      current-prefix-arg)))))
    ;; collect docstring
    (when (stringp (car body))
      (setq doc (pop body)))
    ;; collect keywords
    (while (keywordp (car-safe body))
      (setq key (pop body)
            arg (pop body))
      (setq keys (append keys (list key arg))))
    ;; collect `interactive' specification
    (when (eq (car-safe (car-safe body)) 'interactive)
      (setq interactive `(append ,interactive ,@(cdr (pop body)))))
    ;; macro expansion
    `(progn
       (add-to-list 'evil-motions ',motion t)
       (evil-define-command ,motion (,@args)
         ,@(when doc `(,doc)) ; avoid nil before `interactive'
         ,@keys
         :keep-visual t
         :repeatable nil
         (interactive
          ,@(when interactive
              `(,interactive)))
         ,@body))))

(defmacro evil-signal-without-movement (&rest body)
  "Catches errors point has not changed within this scope."
  (declare (indent defun))
  `(let ((p (point)))
     (condition-case err
         (progn ,@body)
       (error
        (when (= p (point))
          (signal (car err) (cdr err)))))))

(defmacro evil-narrow-to-line (&rest body)
  "Narrows to the current line."
  (declare (indent defun))
  `(save-restriction
     (narrow-to-region
      (line-beginning-position)
      (if (evil-visual-state-p)
          (line-end-position)
        (max (line-beginning-position)
             (1- (line-end-position)))))
     (evil-signal-without-movement ,@body)))

(defun evil-eobp ()
  "Returns t iff (point) is at end-of-buffer w.r.t. end-of-line."
  (or (eobp)
      (and (not (evil-visual-state-p))
           (= (point) (1- (point-max)))
           (not (eolp)))))

(defun evil-adjust-eol ()
  "Move (point) one character back if at eol on an non-empty line."
  (when (and (eolp) (not (bolp)))
    (backward-char)))

(evil-define-motion evil-forward-char (count)
  "Move cursor to the right by COUNT characters."
  :type exclusive
  (evil-narrow-to-line (forward-char (or count 1))))

(evil-define-motion evil-backward-char (count)
  "Move cursor to the left by COUNT characters."
  :type exclusive
  (evil-narrow-to-line (backward-char (or count 1))))

;; The purpose of this function is the provide line motions which
;; preserve the column. This is how 'previous-line and 'next-line
;; work, but unfortunately this behaviour is hard coded, i.e., if and
;; only if the last command was one of 'previous-line and 'next-line
;; the column is preserved. Furthermore, in contrast to vim, when we
;; cannot go further those motions move point to the beginning resp.
;; the end of the line (we do never want point to leave its column).
;; The code here comes from simple.el, and I hope it will work in
;; future.
(defun evil-line-move (count)
  "A wrapper for line motions which conserves the column."
  (evil-signal-without-movement
    (setq this-command 'next-line)
    (let ((opoint (point)))
      (unwind-protect
          (with-no-warnings
            (next-line count))
        (cond
         ((> count 0)
          (line-move-finish (or goal-column temporary-goal-column)
                            opoint nil))
         ((< count 0)
          (line-move-finish (or goal-column temporary-goal-column)
                            opoint t)))))))

(evil-define-motion evil-previous-line (count)
  "Move the cursor COUNT lines up."
  :type line
  (let (line-move-visual)
    (evil-line-move (- (or count 1)))))

(evil-define-motion evil-next-line (count)
  "Move the cursor COUNT lines down."
  :type line
  (let (line-move-visual)
    (evil-line-move (or count 1))))

;; This motion can be used for repeated commands like "dd"
(evil-define-motion evil-line (count)
  "Moves count - 1 lines down."
  :type line
  (let (line-move-visual)
    (evil-line-move (1- (or count 1)))))

(evil-define-motion evil-previous-visual-line (count)
  "Move the cursor COUNT screen lines down."
  :type exclusive
  (let ((line-move-visual t))
    (evil-line-move (- (or count 1)))))

(evil-define-motion evil-next-visual-line (count)
  "Move the cursor COUNT screen lines up."
  :type exclusive
  (let ((line-move-visual t))
    (evil-line-move (or count 1))))

(evil-define-motion evil-move-to-window-line (count)
  "Moves the cursor to line COUNT from the top of the window
on the first non-blank character."
  :type line
  (move-to-window-line (or count 0))
  (back-to-indentation))

(evil-define-motion evil-move-to-middle-window-line ()
  "Moves the cursor to the middle line of the current window
on the first non-blank character."
  :type line
  (move-to-window-line (/ (window-body-height) 2))
  (back-to-indentation))

(evil-define-motion evil-move-to-last-window-line (count)
  "Moves the cursor to line COUNT from the bottom of the window
on the first non-blank character."
  :type line
  (move-to-window-line (- (window-body-height) (or count 0) 1))
  (back-to-indentation))

(evil-define-motion evil-beginning-of-line ()
  "Move the cursor to the beginning of the current line."
  :type exclusive
  (beginning-of-line))

(evil-define-motion evil-beginning-of-line-or-digit-argument ()
  "Move the cursor to the beginning of the current line.
This function passes its command to `digit-argument' (usually a 0)
if it is not the first event."
  :type exclusive
  (if current-prefix-arg
      (progn
        (setq this-command 'digit-argument)
        (call-interactively 'digit-argument))
    (setq this-command 'evil-beginning-of-line)
    (call-interactively 'evil-beginning-of-line)))

(evil-define-motion evil-first-non-blank ()
  "Move the cursor to the first non-blank character of the current line."
  :type exclusive
  (evil-narrow-to-line (back-to-indentation)))

(evil-define-motion evil-end-of-line (count)
  "Move the cursor to the end of the current line.
If COUNT is given, move COUNT - 1 lines downward first."
  :type inclusive
  (end-of-line count)
  (unless (or (evil-visual-state-p)
              (bolp))
    (backward-char)))

(evil-define-motion evil-last-non-blank (count)
  "Move the cursor to the last non-blank character of the current line.
If COUNT is given, move COUNT - 1 lines downward first."
  :type inclusive
  (goto-char
   (save-excursion
     (beginning-of-line count)
     (if (re-search-forward "[ \t]*$")
         (max (line-beginning-position)
              (1- (match-beginning 0)))
       (line-beginning-position)))))

(evil-define-motion evil-move-to-first-non-blank-beg (count)
  "Moves the cursor to the first non-blank character of line COUNT.
By default the first line."
  :type line
  (goto-char (point-min))
  (when count
    (forward-line (1- count)))
  (evil-first-non-blank))

(evil-define-motion evil-move-to-first-non-blank-end (count)
  "Moves the cursor to the first non-blank character of line
COUNT, default the last line."
  :type line
  (if count
      (evil-move-to-first-non-blank-beg count)
    (goto-char (point-max))
    (evil-first-non-blank)))

(evil-define-motion evil-beginning-of-visual-line ()
  "Move the cursor to the first character of the current screen line."
  :type exclusive
  (beginning-of-visual-line))

(evil-define-motion evil-first-non-blank-of-visual-line ()
  "Move the cursor to the first non blank character
of the current screen line."
  :type exclusive
  (evil-beginning-of-visual-line)
  (skip-chars-forward " \t\r"))

(evil-define-motion evil-end-of-visual-line (count)
  "Move the cursor to the last character of the current screen line.
If COUNT is given, move COUNT - 1 screen lines downward first."
  :type inclusive
  (end-of-visual-line count)
  (unless (or (evil-visual-state-p)
              (bolp))
    (backward-char)))

;;; Text object and movement framework
;; Usual text objects like words, WORDS, paragraphs and sentences are
;; defined via a corresponding move-function. The move function must
;; have the following properties:
;;
;; 1. Take exactly one argument, the count.
;;
;; 2. When count is positive, move point forward to the first
;;    character after the end of the next count-th object.
;;
;; 3. When count is negative, move point backward to the first
;;    character of the count-th previous object.
;;
;; 4. If point is placed on the first character of an object, the
;;    backward motion does *not* count that object.
;;
;; 5. If point is placed on the last character of an object, the
;;    forward motion *does* count that object.
;;
;; 6. The return value is count left, i.e., in forward direction count
;;    is decreased by one for each successful move, in backward
;;    direction count is increased by one for each successful move,
;;    the final value of count is returned. Therefore, if the complete
;;    move is successful the return value is 0.
;;
;; After a forward motion point has to be placed on the first
;; character after some object unless no motion was possible at all.
;; Similar, after a backward motion point has to be placed on the
;; first character of some object unless no motion was possible at
;; all. This implies that point should *never* be moved to eob or bob
;; unless an object ends or begins at eob or bob. (Usually, Emacs
;; motions always move as far as possible. But we want to use the
;; motion-function to identify certain objects in the buffer and thus
;; exact movement to object boundaries is required).

(defun evil-move-chars (chars count)
  "Moves point to the end or beginning of a sequence of CHARS.
CHARS is a character set as in [...] of regular expressions."
  (let ((re (concat "[" chars "]")))
    (setq count (or count 1))
    (while (and (> count 0)
                (re-search-forward re nil t))
      (skip-chars-forward chars)
      (setq count (1- count)))
    (while (and (< count 0)
                (re-search-backward re nil t))
      (skip-chars-backward chars)
      (setq count (1+ count)))
    count))

(defmacro evil-define-union-move (name &rest moves)
  "Creates a move which moves to the next object boundary defined
by one movement function in MOVES.

MOVES is a list whose elements have the form (FUNC PARAMS...).
The union move calls (FUNC PARAMS... COUNT). The return value is
the number of moves that could not be performed."
  (declare (indent defun))
  `(defun ,name (count)
     ,@(and moves (listp moves)
            (stringp (car moves))
            (list (pop moves)))
     (setq count (or count 1))
     (catch 'done
       (while (> count 0)
         (let ((results
                (remq nil
                      (list ,@(mapcar
                               #'(lambda (move)
                                   `(save-excursion
                                      (when (zerop ,(append move '(1)))
                                        (point))))
                               moves)))))
           (unless results (throw 'done count))
           (goto-char (apply #'min results))
           (setq count (1- count))))
       (while (< count 0)
         (let ((results
                (remq nil
                      (list ,@(mapcar
                               #'(lambda (move)
                                   `(save-excursion
                                      (when (zerop ,(append move '(-1)))
                                        (point))))
                               moves)))))
           (unless results (throw 'done count))
           (goto-char (apply #'max results))
           (setq count (1+ count))))
       count)))

(defun evil-move-forward-end (move &optional count count-current)
  "Moves point the the COUNT next end of the object specified by
move MOVE. If there are no COUNT objects move the point to the
end of the last object. If there no next object raises
'end-of-buffer. If COUNT-CURRENT is non-nil, the current object
counts as one move, otherwise the end of the current object is
*not* counted."
  (setq count (or count 1))
  (when (evil-eobp)
    (signal 'end-of-buffer nil))
  (unless count-current (forward-char))
  (setq count (funcall move count))
  (backward-char)
  (unless (or (zerop count) (evil-eobp))
    (setq count (1- count))
    (goto-char (point-max)))
  (evil-adjust-eol)
  count)

(defun evil-move-forward-begin (move count)
  "Moves point the the COUNT next beginning of the object
specified by move MOVE. If there are no COUNT objects move the
point to the beginning of the last object. If there no next
object raises 'end-of-buffer."
  (setq count (or count 1))
  (when (evil-eobp)
    (signal 'end-of-buffer nil))
  (let ((opoint (point)))
    ;; goto to the end of the current or the next object
    (when (zerop (funcall move 1))
      ;; check if point we started at the first object
      (when (> (save-excursion (funcall move -1) (point)) opoint)
        ;; no
        (setq count (1- count)))
      (setq count (funcall move count))
      ;; go back to beginning of object
      (funcall move -1))
    (unless (or (zerop count) (evil-eobp))
      (setq count (1- count))
      (goto-char (point-max))))
  (evil-adjust-eol)
  count)

(defun evil-move-backward-begin (move &optional count)
  "Moves point the the COUNT previous beginning of the object
specified by move MOVE. If there are no COUNT objects move the
point to the beginning of the first object. If there no previous
object raises 'beginning-of-buffer."
  (setq count (- (or count 1)))
  (when (bobp)
    (signal 'beginning-of-buffer nil))
  (setq count (funcall move count))
  (unless (or (zerop count) (bobp))
    (setq count (1+ count))
    (goto-char (point-min)))
  (- count))

(defun evil-move-backward-end (move count)
  "Moves point the the COUNT previous end of the object specified
by move MOVE. If there are no COUNT objects move the point to
the end of the first object. If there no previous object raises
'beginning-of-buffer."
  (setq count (- (or count 1)))
  (when (bobp)
    (signal 'beginning-of-buffer nil))
  (let ((opoint (point)))
    ;; goto to the beginning of the current or the previous object
    (when (zerop (funcall move -1))
      ;; check if point we started at the first object
      (when (<= (save-excursion (funcall move 1) (point)) opoint)
        ;; no
        (setq count (1+ count)))
      (setq count (funcall move count))
      ;; go to end of object
      (funcall move 1)
      (backward-char))
    (unless (or (zerop count) (bobp))
      (setq count (1+ count))
      (goto-char (point-min))))
  (- count))

(defun evil-move-empty-lines (count)
  "Moves to the next or previous empty line, repeated COUNT times."
  (setq count (or count 1))
  (cond
   ((> count 0)
    (while (and (> count 0)
                (re-search-forward "^$" nil t)
                (not (eobp)))
      (forward-char)
      (setq count (1- count))))
   ((< count 0)
    (while (and (< count 0)
                (not (bobp))
                (or (backward-char) t)
                (re-search-backward "^$" nil t))
      (setq count (1+ count)))))
  count)

(evil-define-union-move evil-move-word
  "Move by words."
  (evil-move-chars evil-word)
  (evil-move-chars (concat "^ \t\r\n" evil-word))
  (evil-move-empty-lines))

(evil-define-motion evil-forward-word-begin (count)
  "Move the cursor the beginning of the COUNT-th next word."
  :type exclusive
  (if (not (eq this-command 'evil-change))
      (evil-move-forward-begin #'evil-move-word count)
    (evil-move-forward-end #'evil-move-word count t)
    (setq evil-this-type 'inclusive)))

(evil-define-motion evil-forward-word-end (count)
  "Move the cursor the end of the COUNT-th next word."
  :type inclusive
  (evil-move-forward-end #'evil-move-word count))

(evil-define-motion evil-backward-word-begin (count)
  "Move the cursor the beginning of the COUNT-th previous word."
  :type exclusive
  (evil-move-backward-begin #'evil-move-word count))

(evil-define-motion evil-backward-word-end (count)
  "Move the cursor the end of the COUNT-th previous word."
  :type inclusive
  (evil-move-backward-end #'evil-move-word count))

(evil-define-union-move evil-move-WORD
  "Move by WORDs."
  (evil-move-chars "^ \t\r\n")
  (evil-move-empty-lines))

(evil-define-motion evil-forward-WORD-begin (count)
  "Move the cursor the beginning of the COUNT-th next WORD."
  :type exclusive
  (if (not (eq this-command 'evil-change))
      (evil-move-forward-begin #'evil-move-WORD count)
    (evil-move-forward-end #'evil-move-WORD count t)
    (setq evil-this-type 'inclusive)))

(evil-define-motion evil-forward-WORD-end (count)
  "Move the cursor the end of the COUNT-th next WORD."
  :type inclusive
  (evil-move-forward-end #'evil-move-WORD count))

(evil-define-motion evil-backward-WORD-begin (count)
  "Move the cursor the beginning of the COUNT-th previous WORD."
  :type exclusive
  (evil-move-backward-begin #'evil-move-WORD count))

(evil-define-motion evil-backward-WORD-end (count)
  "Move the cursor the end of the COUNT-th previous WORD."
  :type inclusive
  (evil-move-backward-end #'evil-move-WORD count))

;; This function is slightly adapted from paragraphs.el
(defun evil-move-sentence (count)
  "Move by sentence."
  (setq count (or count 1))
  (catch 'done
    (let ((opoint (point))
          (sentence-end (sentence-end)))
      ;; backward
      (while (and (< count 0) (not (bobp)))
        (let ((pos (point))
              (par-beg (save-excursion
                         (and (zerop (evil-move-paragraph -1))
                              (point)))))
          (cond
           ((and (re-search-backward sentence-end par-beg t)
                 (or (< (match-end 0) pos)
                     (re-search-backward sentence-end par-beg t)))
            (goto-char (match-end 0)))
           (par-beg
            (goto-char par-beg))
           (t
            (goto-char pos)
            (throw 'done count))))
        (setq count (1+ count)))
      ;; forward
      (while (and (> count 0) (not (eobp)))
        (let ((par-end (save-excursion
                         (and (zerop (evil-move-paragraph +1)) (point)))))
          (cond
           ((re-search-forward sentence-end par-end t)
            (skip-chars-backward " \t\n"))
           (par-end
            (goto-char par-end))
           (t
            (throw 'done count))))
        (setq count (1- count)))
      (constrain-to-field nil opoint t)
      count)))

(defun evil-move-paragraph (count)
  "Move by paragraph."
  (setq count (or count 1))
  (catch 'done
    (while (and (< count 0) (not (bobp)))
      (let ((opoint (point)) npoint)
        (forward-paragraph -1)
        (setq npoint (point))
        (skip-chars-forward " \t\n")
        (when (and (>= (point) opoint) (< npoint opoint))
          (goto-char npoint)
          (forward-paragraph -1)
          (skip-chars-forward " \t\n")
          (when (and (>= (point) opoint) (< npoint opoint))
            (goto-char opoint)
            (throw 'done count))))
      (setq count (1+ count)))
    (while (and (> count 0) (not (eobp)))
      (let ((opoint (point)) npoint)
        (forward-paragraph +1)
        (setq npoint (point))
        (skip-chars-backward " \t\n")
        (when (<= (point) opoint)
          (goto-char npoint)
          (forward-paragraph +1)
          (skip-chars-backward " \t\n")
          (when (<= (point) opoint)
            (goto-char opoint)
            (throw 'done count))))
      (setq count (1- count)))
    count))

(evil-define-motion evil-forward-sentence (count)
  :type exclusive
  "Moves to the next COUNT-th beginning of a sentence or end of a paragraph."
  (setq count (or count 1))
  (if (evil-eobp)
      (signal 'end-of-buffer nil)
    (while (and (> count 0)
                (not (evil-eobp)))
      (let ((beg-sentence
             (save-excursion
               (and (zerop (evil-move-forward-begin #'evil-move-sentence 1))
                    (point))))
            (end-par
             (save-excursion
               (forward-paragraph)
               (point))))
        (goto-char (apply #'min
                          (remq nil (list beg-sentence end-par))))
        (setq count (1- count))))))

(evil-define-motion evil-backward-sentence (count)
  :type exclusive
  "Moves to the previous COUNT-th beginning of a sentence or paragraph."
  (setq count (or count 1))
  (if (bobp)
      (signal 'beginning-of-buffer nil)
    (while (and (> count 0)
                (not (bobp)))
      (let ((beg-sentence
             (save-excursion
               (and (zerop (evil-move-backward-begin #'evil-move-sentence 1))
                    (point))))
            (beg-par
             (save-excursion
               (backward-paragraph)
               (point))))
        (goto-char (apply #'max
                          (remq nil (list beg-sentence beg-par))))
        (setq count (1- count))))))

(evil-define-motion evil-forward-paragraph (count)
  "Moves to the end of the COUNT-th next paragraph."
  :type exclusive
  (if (evil-eobp)
      (signal 'end-of-buffer nil)
    (forward-paragraph count)))

(evil-define-motion evil-backward-paragraph (count)
  "Moves to the beginning of the COUNT-th previous paragraph."
  :type exclusive
  (if (bobp)
      (signal 'beginning-of-buffer nil)
    (backward-paragraph count)))

(evil-define-motion evil-find-char (count char)
  "Move the cursor to the next COUNT'th occurrence of character CHAR."
  :type inclusive
  (interactive (list (read-char)))
  (setq count (or count 1))
  (let ((fwd (> count 0)))
    (setq evil-last-find (list #'evil-find-char char fwd))
    (when fwd (forward-char))
    (let ((case-fold-search nil))
      (unless (prog1
                  (search-forward (char-to-string char)
                                  (unless evil-find-skip-newlines
                                    (if fwd
                                        (line-end-position)
                                      (line-beginning-position)))
                                  t count)
                (when fwd (backward-char)))
        (error "Can't find %c" char)))))

(evil-define-motion evil-find-char-backward (count char)
  "Move the cursor to the next COUNT'th occurrence of character CHAR."
  :type exclusive
  (interactive (list (read-char)))
  (evil-find-char (- (or count 1)) char))

(evil-define-motion evil-find-char-to (count char)
  "Move the cursor to the character before the next COUNT'th occurence of arg."
  :type inclusive
  (interactive (list (read-char)))
  (unwind-protect
      (progn
        (evil-find-char count char)
        (if (> (or count 1) 0)
            (backward-char)
          (forward-char)))
    (setcar evil-last-find #'evil-find-char-to)))

(evil-define-motion evil-find-char-to-backward (count char)
  "Move the cursor to the character before the previous COUNT'th occurence of arg."
  :type exclusive
  (interactive (list (read-char)))
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
        (funcall cmd (if fwd count (- count)) char)
        (unless (nth 2 evil-last-find)
          (setq evil-this-type 'exclusive)))
    (error "No previous search")))

(evil-define-motion evil-repeat-find-char-reverse (count)
  "Repeat the last find COUNT times in the opposite direction."
  :type inclusive
  (evil-repeat-find-char (- (or count 1))))

;; TODO: this is a very basic implementation considering only (),[],{}
;; and not blocks like #if #endif.
(evil-define-motion evil-jump-item (count)
  "Find the next item in this line after or under the cursor and
jumps to the corresponding one."
  :type inclusive
  (let ((next-open
         (condition-case err
             (1- (scan-lists (point) 1 -1))
           (error
            (point-max))))
        (next-close
         (condition-case nil
             (1- (scan-lists (point) 1 +1))
           (error (point-max)))))
    (let ((pos (min next-open next-close)))
      (when (>= pos (line-end-position))
        (error "No matching item found on the current line."))
      (if (= pos next-open)
          (progn
            (goto-char pos)
            (forward-list)
            (backward-char))
        (progn
          (goto-char (1+ pos))
          (backward-list))))))

(provide 'evil-motions)

;;; evil-motions.el ends here
