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
  (let (interactive keyword type)
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
      (setq keyword (pop body))
      (cond
       ((eq keyword :type)
        (setq type (pop body)))
       (t
        (pop body))))
    ;; collect `interactive' specification
    (when (eq (car-safe (car-safe body)) 'interactive)
      (setq interactive `(append ,interactive ,@(cdr (pop body)))))
    ;; macro expansion
    `(progn
       (add-to-list 'evil-motions ',motion t)
       (when ',type
         (evil-set-type ',motion ',type))
       (defun ,motion (,@args)
         ,@(when doc `(,doc)) ; avoid nil before `interactive'
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
  (if count
      (goto-line count)
    (goto-char (point-min)))
  (evil-first-non-blank))

(evil-define-motion evil-move-to-first-non-blank-end (count)
  "Moves the cursor to the first non-blank character of line
COUNT, default the last line."
  :type line
  (if count
      (goto-line count)
    (goto-char (point-max)))
  (evil-first-non-blank))

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


;; Text object and movement framework

(defun evil-negate-chars (chars)
  "Returns the negated character set of CHARS."
  (if (= (aref chars 0) ?^)
      (substring chars 1)
    (concat "^" chars)))

(defun evil-select-chars (chars count)
  "Returns the position of the last character in the next sequence of CHARS.

DIRECTION is either 'fwd or 'bwd. If DIRECTION is 'fwd the
function returns the position of the last character in the next
consecutive sequence of CHARS. If DIRECTION is 'bwd the function
returns the position of the first character in the next sequence
of CHARS in backward direction. If the character at point is one
of CHARS then this sequence is used. If no position can be found
the function returns nil.

CHARS is a character set like inside of a `[...]' in a regular
expression."
  (setq count (or count 1))
  (catch 'done
    (cond
     ((> count 0)
      (dotimes (i count)
        (if (re-search-forward (concat "[" chars "]") nil t)
            (skip-chars-forward chars)
          (throw 'done (- count i)))))
     ((< count 0)
      (dotimes (i (- count))
        (if (re-search-backward (concat "[" chars "]") nil t)
            (skip-chars-backward chars)
          (throw 'done (+ count i))))))
    0))

(defmacro evil-define-union-select (name &rest selectors)
  "Creates a selector function NAME which returns the next or
previous position of one of SELECTORS. NAME is the name of the
function to be defined.

SELECTORS is a list whose elements have the form (FUNC
PARAMS...).  The union selector calls (FUNC direction PARAMS...)
for each element and returns the smallest (in forward direction)
or largest (in backward direction) value returned by those
selectors. If no selector returns a value nil is returned."
  (declare (indent defun))
  `(defun ,name (count)
     ,@(and selectors (listp selectors)
            (stringp (car selectors))
            (list (pop selectors)))
     (setq count (or count 1))
     (catch 'done
       (cond
        ((> count 0)
         (while (> count 0)
           (let ((results
                  (remq nil
                        (list ,@(mapcar
                                 #'(lambda (sel)
                                     `(save-excursion
                                        (when (zerop ,(append sel '(1)))
                                          (point))))
                                 selectors)))))
             (unless results (throw 'done count))
             (goto-char (apply #'min results))
             (setq count (1- count)))))
        ((< count 0)
         (while (< count 0)
           (let ((results
                  (remq nil
                        (list ,@(mapcar
                                 #'(lambda (sel)
                                     `(save-excursion
                                        (when (zerop ,(append sel '(-1)))
                                          (point))))
                                 selectors)))))
             (unless results (throw 'done count))
             (goto-char (apply #'max results))
             (setq count (1+ count))))))
       count)))

(defun evil-select-forward-end (sel &optional count)
  "Moves point the the COUNT next end of the object specified by
selector SEL. If there are no COUNT objects move the point to the
end of the last object. If there no next object raises
'end-of-buffer."
  (setq count (or count 1))
  (when (eobp)
    (signal 'end-of-buffer nil))
  (prog1
      (forward-char)
    (let ((rest (funcall sel count)))
      (when (= rest count)
        (signal 'end-of-buffer nil))
      rest)
    (backward-char)))

(defun evil-select-forward-begin (sel count)
  "Moves point the the COUNT next beginning of the object
specified by selector SEL. If there are no COUNT objects move the
point to the beginning of the last object. If there no next
object raises 'end-of-buffer."
  (setq count (or count 1))
  (prog1
      (let ((start (point)))
        ;; goto to the end of the current or the next object
        (unless (zerop (funcall sel 1))
          (signal 'end-of-buffer nil))
        ;; check if point we started at the first object
        (if (> (save-excursion
                 (funcall sel -1)
                 (point))
               start)
            ;; no
            (funcall sel (1- count))
          ;; yes
          (let ((rest (funcall sel count)))
            (when (= rest count)
              (signal 'end-of-buffer nil))
            rest)))
    (funcall sel -1)))

(defun evil-select-backward-begin (sel &optional count)
  "Moves point the the COUNT previous beginning of the object
specified by selector SEL. If there are no COUNT objects move the
point to the beginning of the first object. If there no previous
object raises 'beginning-of-buffer."
  (setq count (- (or count 1)))
  (let ((rest (funcall sel count)))
    (when (= rest count)
      (signal 'beginning-of-buffer nil))
    (- rest)))

(defun evil-select-backward-end (sel count)
  "Moves point the the COUNT previous end of the object specified
by selector SEL. If there are no COUNT objects move the point to
the end of the first object. If there no previous object raises
'beginning-of-buffer."
  (setq count (- (or count 1)))
  (prog1
      (let ((start (point)))
        ;; goto to the end of the current or the next object
        (unless (zerop (funcall sel -1))
          (signal 'beginning-of-buffer nil))
        ;; check if point we started at the first object
        (if (< (save-excursion
                 (funcall sel +1)
                 (1- (point)))
               start)
            ;; no
            (funcall sel (1+ count))
          ;; yes
          (let ((rest (funcall sel count)))
          (when (= rest count)
            (signal 'beginning-of-buffer nil))
          rest)))
    (funcall sel +1)
    (backward-char)))


(defun evil-select-empty-lines (count)
  "Returns the position of the next or previous empty line."
  (setq count (or count 1))
  (cond
   ((> count 0)
    (while (and (> count 0)
                (re-search-forward "^$" nil t))
      (forward-char)
      (setq count (1- count))))
   ((< count 0)
    (while (and (< count 0)
                (not (bobp))
                (or (backward-char) t)
                (re-search-backward "^$" nil t))
      (setq count (1+ count)))))
  count)


(evil-define-union-select evil-select-word
  "Selector for a word."
  (evil-select-chars evil-word)
  (evil-select-chars (concat "^ \t\r\n" evil-word))
  (evil-select-empty-lines))

(evil-define-motion evil-forward-word-begin (count)
  "Move the cursor the beginning of the COUNT-th next word."
  :type exclusive
  (evil-select-forward-begin #'evil-select-word count))

(evil-define-motion evil-forward-word-end (count)
  "Move the cursor the end of the COUNT-th next word."
  :type inclusive
  (evil-select-forward-end #'evil-select-word count))

(evil-define-motion evil-backward-word-begin (count)
  "Move the cursor the beginning of the COUNT-th previous word."
  :type exclusive
  (evil-select-backward-begin #'evil-select-word count))

(evil-define-motion evil-backward-word-end (count)
  "Move the cursor the end of the COUNT-th previous word."
  :type inclusive
  (evil-select-backward-end #'evil-select-word count))


(evil-define-union-select evil-select-WORD
  "Selector for a WORD."
  (evil-select-chars "^ \t\r\n")
  (evil-select-empty-lines))

(evil-define-motion evil-forward-WORD-begin (count)
  "Move the cursor the beginning of the COUNT-th next WORD."
  :type exclusive
  (evil-select-forward-begin #'evil-select-WORD count))

(evil-define-motion evil-forward-WORD-end (count)
  "Move the cursor the end of the COUNT-th next WORD."
  :type inclusive
  (evil-select-forward-end #'evil-select-WORD count))

(evil-define-motion evil-backward-WORD-begin (count)
  "Move the cursor the beginning of the COUNT-th previous WORD."
  :type exclusive
  (evil-select-forward-begin #'evil-select-WORD count))

(evil-define-motion evil-backward-WORD-end (count)
  "Move the cursor the end of the COUNT-th previous WORD."
  :type inclusive
  (evil-select-forward-end #'evil-select-WORD count))


(defun evil-select-sentence-1 (count)
  "Select a sentence."
  (setq count (or count 1))
  (cond
   ((> count 0)
    (while (and (> count 0)
                (re-search-forward (sentence-end) nil t))
      (goto-char (1+ (match-beginning 0)))
      (setq count (1- count))))
   ((< count 0)
    (let ((p (point))
          (orig-count count))
      (while (and (< count 0)
                  (re-search-backward (sentence-end) nil t))
        (when (< (match-end 0) p)
          (setq count (1+ count))))
      (if (= orig-count count)
          (goto-char p)
        (re-search-forward (sentence-end) nil t)))))
  count)

(defun evil-select-paragraph (count)
  "Selector for a paragraph."
  (let* ((p (point))
         (rest (forward-paragraph count)))
    (when (and (= p (point))
               (not (looking-at paragraph-start)))
      (if (> count 0)
          (setq rest (1- rest))
        (setq rest (1+ rest))))
    rest))

(evil-define-union-select evil-select-sentence
  "Selector for a sentence."
  (evil-select-sentence-1)
  (evil-select-paragraph))

(evil-define-motion evil-forward-sentence-begin (count)
  :type exclusive
  (evil-select-forward-begin #'evil-select-sentence count))

(evil-define-motion evil-backward-sentence-begin (count)
  :type exclusive
  (evil-select-backward-begin #'evil-select-sentence count))

(evil-define-motion evil-forward-paragraph-begin (count)
  :type exclusive
  (evil-select-forward-begin #'evil-select-paragraph count))

(evil-define-motion evil-backward-paragraph-begin (count)
  :type exclusive
  (evil-select-backward-begin #'evil-select-paragraph count))

(provide 'evil-motions)

;;; evil-motions.el ends here
