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

(defun evil-select-chars (direction chars)
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
  (save-excursion
    (cond
     ((eq direction 'fwd)
      (skip-chars-forward (evil-negate-chars chars))
      (unless (eobp)
        (skip-chars-forward chars)
        (backward-char)
        (point)))
     (t
      (unless (eobp) (forward-char))
      (skip-chars-backward (evil-negate-chars chars))
      (unless (bobp)
        (skip-chars-backward chars)
        (point))))))

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
  `(defun ,name (direction)
     ,@(and selectors (listp selectors)
            (stringp (car selectors))
            (list (pop selectors)))
     (let ((results
            (remq nil
                  (list ,@(mapcar #'(lambda (move)
                                      (cons (car move) (cons 'direction (cdr move))))
                                  selectors)))))
       (when results
         (apply (if (eq direction 'fwd) #'min #'max) results)))))

(defun evil-select-forward-end (sel &optional count)
  "Moves point the the COUNT next end of the object specified by
selector SEL. If there are no COUNT objects move the point to the
end of the last object. If there no next object raises
'end-of-buffer."
  (catch 'done
    (dotimes (i (or count 1))
      (let (p)
        ;; go to end of next object
        (save-excursion
          (unless (eobp)
            (forward-char)
            (setq p (funcall sel 'fwd))))
        ;; if sucessful move point ...
        (if p (goto-char p)
          ;; otherwise raise error (if first iteration) or return
          ;; number of successful moves
          (when (zerop i)
            (signal 'end-of-buffer nil))
          (throw 'done i))))
    ;; moved completely
    count))

(defun evil-select-forward-begin (sel count)
  "Moves point the the COUNT next beginning of the object
specified by selector SEL. If there are no COUNT objects move the
point to the beginning of the last object. If there no next
object raises 'end-of-buffer."
  (setq count (or count 1))
  (prog1
      (catch 'done
        (let ((start (point))
              (at-object t); t iff we start at an object
              p)
          ;; goto to the end of the current or the next object
          (unless (setq p (funcall sel 'fwd))
            (signal 'end-of-buffer nil))
          (goto-char p)
          ;; check if point we started at the first object
          (when (> (funcall sel 'bwd) start)
            ;; no
            (setq count (1- count)
                  at-object nil))
          (dotimes (i count)
            ;; go to end of next object
            (save-excursion
              (unless (eobp)
                (forward-char)
                (setq p (funcall sel 'fwd))))
            ;; if successful move point
            (if p (goto-char p)
              ;; otherwise raise error (if first iteration) or return
              ;; number of successful moves. Note that we have to
              ;; respect if we started on an object (at-object).
              (when (and at-object (zerop i))
                (goto-char start)
                (signal 'end-of-buffer nil))
              (throw 'done (if at-object i (1+ i)))))
          ;; moved completely
          (if at-object count (1+ count))))
    ;; go back to the beginning of the found object
    (goto-char (funcall sel 'bwd))))

(defun evil-select-backward-begin (sel &optional count)
  "Moves point the the COUNT previous beginning of the object
specified by selector SEL. If there are no COUNT objects move the
point to the beginning of the first object. If there no previous
object raises 'beginning-of-buffer."
  (catch 'done
    (dotimes (i (or count 1))
      (let (p)
        ;; go to beginning of previous object
        (save-excursion
          (unless (bobp)
            (borward-char)
            (setq p (funcall sel 'bwd))))
        ;; if sucessful move point ...
        (if p (goto-char p)
          ;; otherwise raise error (if first iteration) or return
          ;; number of successful moves
          (when (zerop i)
            (signal 'beginning-of-buffer nil))
          (throw 'done i))))
    ;; moved completely
    count))

(defun evil-select-backward-end (sel count)
  "Moves point the the COUNT previous end of the object specified
by selector SEL. If there are no COUNT objects move the point to
the end of the first object. If there no previous object raises
'beginning-of-buffer."
  (setq count (or count 1))
  (prog1
      (catch 'done
        (let ((start (point))
              (at-object t); t iff we start at an object
              p)
          ;; goto to the beginning of the current or the previous object
          (unless (setq p (funcall sel 'bwd))
            (signal 'beginning-of-buffer nil))
          (goto-char p)
          ;; check if point we started at the first object
          (when (< (funcall sel 'fwd) start)
            ;; no
            (setq count (1- count)
                  at-object nil))
          (dotimes (i count)
            ;; go to end of previous object
            (save-excursion
              (unless (eobp)
                (forward-char)
                (setq p (funcall sel 'bwd))))
            ;; if successful move point
            (if p (goto-char p)
              ;; otherwise raise error (if first iteration) or return
              ;; number of successful moves. Note that we have to
              ;; respect if we started on an object (at-object).
              (when (and at-object (zerop i))
                (goto-char start)
                (signal 'beginning-of-buffer nil))
              (throw 'done (if at-object i (1+ i)))))
          ;; moved completely
          (if at-object count (1+ count))))
    ;; go back to the end of the found object
    (goto-char (funcall sel 'fwd))))

(defun evil-select-empty-lines (direction)
  "Returns the position of the next or previous empty line."
  (save-excursion
    (let ((dir (if (eq direction 'fwd) +1 -1)))
      (while (and (not (and (bolp) (eolp)))
                  (zerop (forward-line dir))))
      (and (bolp) (eolp) (point)))))

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

(provide 'evil-motions)

;;; evil-motions.el ends here
