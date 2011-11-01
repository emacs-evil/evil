;;;; Motions

(require 'evil-common)
(require 'evil-visual)
(require 'evil-operators)

(evil-define-state motion
  "Motion state."
  :tag " <M> "
  :suppress-keymap t)

(defmacro evil-define-motion (motion args &rest body)
  "Define an motion command MOTION.

\(fn MOTION (COUNT ARGS...) DOC [[KEY VALUE]...] BODY...)"
  (declare (indent defun)
           (debug (&define name lambda-list
                           [&optional stringp]
                           [&rest keywordp sexp]
                           [&optional ("interactive" [&rest form])]
                           def-body)))
  (let (arg doc interactive jump key keys type)
    (when args
      (setq args `(&optional ,@(delq '&optional args))
            ;; the count is either numerical or nil
            interactive '("<c>")))
    ;; collect docstring
    (when (and (> (length body) 1)
               (or (eq (car-safe (car-safe body)) 'format)
                   (stringp (car-safe body))))
      (setq doc (pop body)))
    ;; collect keywords
    (while (keywordp (car-safe body))
      (setq key (pop body)
            arg (pop body))
      (cond
       ((eq key :jump)
        (setq jump arg))
       (t
        (setq keys (append keys (list key arg))))))
    ;; collect `interactive' specification
    (when (eq (car-safe (car-safe body)) 'interactive)
      (setq interactive (cdr (pop body))))
    (when interactive
      (setq interactive (apply 'evil-interactive-form interactive))
      (setq keys (append keys (cdr-safe interactive))
            interactive (car-safe interactive)))
    ;; macro expansion
    `(progn
       ;; refresh echo area in Eldoc mode
       (when ',motion
         (eval-after-load 'eldoc
           '(eldoc-add-command ',motion)))
       (evil-define-command ,motion (,@args)
         ,@(when doc `(,doc))          ; avoid nil before `interactive'
         ,@keys
         :keep-visual t
         :repeat motion
         (interactive
          ,@(when (or jump interactive)
              `((progn
                  ,(when jump
                     '(unless (or (evil-visual-state-p)
                                  (evil-operator-state-p))
                        (evil-set-jump)))
                  ,interactive))))
         ,@body))))

(defmacro evil-motion-loop (spec &rest body)
  "Loop a certain number of times.
Evaluate BODY repeatedly COUNT times with VAR bound to 1 or -1,
depending on the sign of COUNT. RESULT, if specified, holds
the number of unsuccessful iterations, which is 0 if the loop
completes successfully. This is also the return value.

Each iteration must move point; if point does not change,
the loop immediately quits. See also `evil-loop'.

\(fn (VAR COUNT [RESULT]) BODY...)"
  (declare (indent defun)
           (debug ((symbolp form &optional symbolp) body)))
  (let* ((var (or (pop spec) (make-symbol "unitvar")))
         (countval (or (pop spec) 0))
         (result (pop spec))
         (i (make-symbol "loopvar"))
         (count (make-symbol "countvar"))
         (done (make-symbol "donevar"))
         (orig (make-symbol "origvar")))
    `(let* ((,count ,countval)
            (,var (if (< ,count 0) -1 1)))
       (catch ',done
         (evil-loop (,i ,count ,result)
           (let ((,orig (point)))
             ,@body
             (when (= (point) ,orig)
               (throw ',done ,i))))))))

(defmacro evil-signal-without-movement (&rest body)
  "Catches errors provided point moves within this scope."
  (declare (indent defun)
           (debug t))
  `(let ((p (point)))
     (condition-case err
         (progn ,@body)
       (error
        (when (= p (point))
          (signal (car err) (cdr err)))))))

(defmacro evil-narrow-to-line (&rest body)
  "Narrow BODY to the current line."
  (declare (indent defun)
           (debug t))
  `(save-restriction
     (narrow-to-region
      (line-beginning-position)
      (if (and evil-move-cursor-back
               (evil-normal-state-p))
          (max (line-beginning-position)
               (1- (line-end-position)))
        (line-end-position)))
     (evil-signal-without-movement
       (condition-case nil
           (progn ,@body)
         (beginning-of-buffer
          (error "Beginning of line"))
         (end-of-buffer
          (error "End of line"))))))

(defmacro evil-narrow-to-line-if (cond &rest body)
  "Narrow BODY to the current line if COND yields non-nil."
  (declare (indent 1)
           (debug t))
  `(if ,cond
       (evil-narrow-to-line ,@body)
     ,@body))

(defun evil-goto-min (&rest positions)
  "Go to the smallest position in POSITIONS.
Non-numerical elements are ignored.
See also `evil-goto-max'."
  (when (setq positions (evil-filter-list
                         (lambda (elt)
                           (not (number-or-marker-p elt)))
                         positions))
    (goto-char (apply #'min positions))))

(defun evil-goto-max (&rest positions)
  "Go to the largest position in POSITIONS.
Non-numerical elements are ignored.
See also `evil-goto-min'."
  (when (setq positions (evil-filter-list
                         (lambda (elt)
                           (not (number-or-marker-p elt)))
                         positions))
    (goto-char (apply #'max positions))))

(defun evil-eobp ()
  "Whether point is at end-of-buffer w.r.t. end-of-line."
  (or (eobp)
      (and (evil-normal-state-p)
           (= (point) (1- (point-max)))
           (not (eolp)))))

(evil-define-motion evil-forward-char (count)
  "Move cursor to the right by COUNT characters."
  :type exclusive
  (evil-narrow-to-line-if
      ;; Narrow movement to the current line if `evil-cross-lines'
      ;; is nil. However, do allow the following exception: if
      ;; `evil-move-cursor-back' is nil and we are next to a newline,
      ;; that newline should be available to operators ("dl", "x").
      (and (not evil-cross-lines)
           (or evil-move-cursor-back
               (not (evil-operator-state-p))
               (not (eolp))))
    (evil-motion-loop (nil (or count 1))
      ;; skip newlines when crossing lines in Normal/Motion state
      (when (and evil-cross-lines
                 (not (evil-operator-state-p))
                 (not (eolp))
                 (save-excursion (forward-char) (eolp)))
        (forward-char))
      (forward-char))))

(evil-define-motion evil-backward-char (count)
  "Move cursor to the left by COUNT characters."
  :type exclusive
  (evil-narrow-to-line-if
      ;; narrow movement to the current line if `evil-cross-lines'
      ;; is nil (cf. `evil-forward-char')
      (and (not evil-cross-lines)
           (or evil-move-cursor-back
               (not (evil-operator-state-p))
               (not (bolp))))
    (evil-motion-loop (nil (or count 1))
      (backward-char)
      ;; adjust cursor when crossing lines in Normal/Motion state
      (unless (evil-operator-state-p)
        (evil-adjust-eol)))))

;; The purpose of this function is the provide line motions which
;; preserve the column. This is how `previous-line' and `next-line'
;; work, but unfortunately the behaviour is hard-coded: if and only if
;; the last command was `previous-line' or `next-line', the column is
;; preserved. Furthermore, in contrast to Vim, when we cannot go
;; further, those motions move point to the beginning resp. the end of
;; the line (we never want point to leave its column). The code here
;; comes from simple.el, and I hope it will work in future.
(defun evil-line-move (count)
  "A wrapper for line motions which conserves the column."
  (evil-signal-without-movement
    (setq this-command 'next-line)
    (let ((opoint (point)))
      (condition-case err
          (with-no-warnings
            (next-line count))
        ((beginning-of-buffer end-of-buffer)
         (let ((col (or goal-column
                        (if (consp temporary-goal-column)
                            (car temporary-goal-column)
                          temporary-goal-column))))
           (if line-move-visual
               (vertical-motion (cons col 0))
             (line-move-finish col opoint (< count 0)))
           ;; maybe we should just (ding) ?
           (signal (car err) (cdr err))))))))

(evil-define-command evil-goto-mark (char)
  "Go to marker denoted by CHAR."
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
  "Go to line of marker denoted by CHAR."
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
     ((and (evil-insert-state-p)
           (not buffer-read-only))
      (if (not evil-auto-indent)
          (newline count)
        (delete-horizontal-space t)
        (newline count)
        (indent-according-to-mode)))
     (t
      (evil-next-line count)))))

;; used for repeated commands like "dd"
(evil-define-motion evil-line (count)
  "Move COUNT - 1 lines down."
  :type line
  (let (line-move-visual)
    (evil-line-move (1- (or count 1)))))

(evil-define-motion evil-previous-visual-line (count)
  "Move the cursor COUNT screen lines up."
  :type exclusive
  (let ((line-move-visual t))
    (evil-line-move (- (or count 1)))))

(evil-define-motion evil-next-visual-line (count)
  "Move the cursor COUNT screen lines down."
  :type exclusive
  (let ((line-move-visual t))
    (evil-line-move (or count 1))))

(evil-define-motion evil-window-top (count)
  "Move the cursor to line COUNT from the top of the window
on the first non-blank character."
  :jump t
  :type line
  (move-to-window-line (or count 0))
  (back-to-indentation))

(evil-define-motion evil-window-middle ()
  "Move the cursor to the middle line of the buffer text currently visible in the window on the first non-blank character."
  :jump t
  :type line
  (move-to-window-line (/ (save-excursion
                            (move-to-window-line -1))
                          2))
  (back-to-indentation))

(evil-define-motion evil-window-bottom (count)
  "Move the cursor to line COUNT from the bottom of the window
on the first non-blank character."
  :jump t
  :type line
  (move-to-window-line (- (or count 1)))
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
  (cond
   (current-prefix-arg
    (setq this-command 'digit-argument)
    (call-interactively 'digit-argument))
   (t
    (setq this-command 'evil-beginning-of-line)
    (call-interactively 'evil-beginning-of-line))))

(evil-define-motion evil-first-non-blank ()
  "Move the cursor to the first non-blank character of the current line."
  :type exclusive
  (evil-narrow-to-line (back-to-indentation)))

(evil-define-motion evil-end-of-line (count)
  "Move the cursor to the end of the current line.
If COUNT is given, move COUNT - 1 lines downward first."
  :type inclusive
  (end-of-line count)
  (unless (evil-visual-state-p)
    (evil-adjust-eol)
    (when (eolp)
      ;; prevent "c$" and "d$" from deleting blank lines
      (setq evil-this-type 'exclusive))))

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

(evil-define-motion evil-previous-line-first-non-blank (count)
  "Move the cursor COUNT lines up on the first non-blank character."
  :type line
  (evil-previous-line (or count 1))
  (evil-first-non-blank))

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

(evil-define-motion evil-goto-first-line (count)
  "Go to the first non-blank character of line COUNT.
By default the first line."
  :jump t
  :type line
  (evil-goto-line (or count 1)))

(evil-define-motion evil-goto-line (count)
  "Go to the first non-blank character of line COUNT.
By default the last line."
  :jump t
  :type line
  (if (null count)
      (goto-char (point-max))
    (goto-char (point-min))
    (forward-line (1- count)))
  (evil-first-non-blank))

(evil-define-motion evil-beginning-of-visual-line ()
  "Move the cursor to the first character of the current screen line."
  :type exclusive
  (if (fboundp 'beginning-of-visual-line)
      (beginning-of-visual-line)
    (beginning-of-line)))

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
  (if (fboundp 'end-of-visual-line)
      (end-of-visual-line count)
    (end-of-line count))
  (unless (evil-visual-state-p)
    (evil-adjust-eol)))

(evil-define-motion evil-jump-to-tag ()
  "Jump to tag under point."
  :jump t
  (let ((tag (thing-at-point 'symbol)))
    (find-tag tag)))

;;; Text object and movement framework

;; Usual text objects like words, WORDS, paragraphs and sentences are
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

(defmacro evil-define-union-move (name args &rest moves)
  "Create a movement function named NAME.
The function moves to the nearest object boundary defined by one
of the movement function in MOVES, which is a list where each
element has the form \(FUNC PARAMS... COUNT).

COUNT is a variable which is bound to 1 or -1, depending on the
direction. In each iteration, the function calls each move in
isolation and settles for the nearest position. If unable to move
further, the return value is the number of iterations that could
not be performed.

\(fn NAME (COUNT) MOVES...)"
  (declare (indent defun)
           (debug (&define name lambda-list
                           [&optional stringp]
                           def-body)))
  (let* ((var (or (car-safe args) 'var))
         (doc (when (stringp (car-safe moves))
                (pop moves)))
         (moves (mapcar #'(lambda (move)
                            `(save-excursion
                               ;; don't include failing moves
                               (when (zerop ,move)
                                 (point))))
                        moves)))
    `(evil-define-motion ,name (count)
       ,@(when doc `(,doc))
       (evil-motion-loop (,var (or count 1))
         (if (> ,var 0)
             (evil-goto-min ,@moves)
           (evil-goto-max ,@moves))))))

(defun evil-move-chars (chars count)
  "Move point to the end or beginning of a sequence of CHARS.
CHARS is a character set as inside [...] in a regular expression."
  (let ((regexp (format "[%s]" chars)))
    (evil-motion-loop (var count)
      (cond
       ((< var 0)
        (re-search-backward regexp nil t)
        (skip-chars-backward chars))
       (t
        (re-search-forward regexp nil t)
        (skip-chars-forward chars))))))

(defun evil-move-beginning (count forward &optional backward)
  "Move to the beginning of the COUNT next object.
If COUNT is negative, move to the COUNT previous object.
FORWARD is a function which moves to the end of the object, and
BACKWARD is a function which moves to the beginning.
If one is unspecified, the other is used with a negative argument."
  (let* ((count (or count 1))
         (backward (or backward
                       (lambda (count)
                         (funcall forward (- count)))))
         (forward (or forward
                      (lambda (count)
                        (funcall backward (- count)))))
         (opoint (point)))
    (cond
     ((< count 0)
      (when (bobp)
        (signal 'beginning-of-buffer nil))
      (unwind-protect
          (evil-motion-loop (nil count count)
            (funcall backward 1))
        (unless (zerop count)
          (goto-char (point-min)))))
     ((> count 0)
      (when (evil-eobp)
        (signal 'end-of-buffer nil))
      ;; Do we need to move past the current object?
      (when (<= (save-excursion
                  (funcall forward 1)
                  (funcall backward 1)
                  (point))
                opoint)
        (setq count (1+ count)))
      (unwind-protect
          (evil-motion-loop (nil count count)
            (funcall forward 1))
        (if (zerop count)
            ;; go back to beginning of object
            (funcall backward 1)
          (goto-char (point-max)))))
     (t
      count))))

(defun evil-move-end (count forward &optional backward inclusive)
  "Move to the end of the COUNT next object.
If COUNT is negative, move to the COUNT previous object.
FORWARD is a function which moves to the end of the object, and
BACKWARD is a function which moves to the beginning.
If one is unspecified, the other is used with a negative argument.
If INCLUSIVE is non-nil, then point is placed at the last character
of the object; otherwise it is placed at the end of the object."
  (let* ((count (or count 1))
         (backward (or backward
                       (lambda (count)
                         (funcall forward (- count)))))
         (forward (or forward
                      (lambda (count)
                        (funcall backward (- count)))))
         (opoint (point)))
    (cond
     ((< count 0)
      (when (bobp)
        (signal 'beginning-of-buffer nil))
      ;; Do we need to move past the current object?
      (when (>= (save-excursion
                  (funcall backward 1)
                  (funcall forward 1)
                  (point))
                (if inclusive
                    (1+ opoint)
                  opoint))
        (setq count (1- count)))
      (unwind-protect
          (evil-motion-loop (nil count count)
            (funcall backward 1))
        (if (not (zerop count))
            (goto-char (point-min))
          ;; go to end of object
          (funcall forward 1)
          (when inclusive
            (unless (bobp) (backward-char)))
          (when (evil-normal-state-p)
            (evil-adjust-eol t)))))
     ((> count 0)
      (when (evil-eobp)
        (signal 'end-of-buffer nil))
      (when inclusive
        (forward-char))
      (unwind-protect
          (evil-motion-loop (nil count count)
            (funcall forward 1))
        (if (not (zerop count))
            (goto-char (point-max))
          (when inclusive
            (unless (bobp) (backward-char)))
          (when (evil-normal-state-p)
            (evil-adjust-eol t)))))
     (t
      count))))

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
  (evil-move-chars (concat "^ \t\r\n" evil-word) count)
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
    (prog1 (if (eq evil-this-operator 'evil-change)
               (evil-move-end count move)
             (evil-move-beginning count move))
      ;; if we reached the beginning of a new line in Operator-Pending
      ;; state, go back to the end of the previous line
      (when (and (evil-operator-state-p)
                 (> (point) orig)
                 (bolp))
        (backward-char)))))

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

;; this function is slightly adapted from paragraphs.el
(defun evil-move-sentence (count)
  "Move by sentence."
  (let ((count (or count 1))
        (opoint (point))
        (sentence-end (sentence-end))
        pos par-beg par-end)
    (evil-motion-loop (var count)
      (cond
       ;; backward
       ((< var 0)
        (setq pos (point)
              par-beg (save-excursion
                        (and (zerop (evil-move-paragraph -1))
                             (point))))
        (if (and (re-search-backward sentence-end par-beg t)
                 (or (< (match-end 0) pos)
                     (re-search-backward sentence-end par-beg t)))
            (goto-char (match-end 0))
          (goto-char (or par-beg pos))))
       ;; forward
       (t
        (setq par-end (save-excursion
                        (and (zerop (evil-move-paragraph 1))
                             (point))))
        (if (re-search-forward sentence-end par-end t)
            (skip-chars-backward " \t\n")
          (goto-char (or par-end (point)))))))))

(defun evil-move-paragraph (count)
  "Move by paragraph."
  (let ((count (or count 1))
        npoint opoint)
    (evil-motion-loop (var count)
      (setq opoint (point))
      (cond
       ((< var 0)
        (forward-paragraph -1)
        (setq npoint (point))
        (skip-chars-forward " \t\n")
        (when (and (>= (point) opoint) (< npoint opoint))
          (goto-char npoint)
          (forward-paragraph -1)
          (skip-chars-forward " \t\n")
          (when (and (>= (point) opoint) (< npoint opoint))
            (goto-char opoint))))
       (t
        (forward-paragraph 1)
        (setq npoint (point))
        (skip-chars-backward " \t\n")
        (when (<= (point) opoint)
          (goto-char npoint)
          (forward-paragraph 1)
          (skip-chars-backward " \t\n")
          (when (<= (point) opoint)
            (goto-char opoint))))))))

(evil-define-motion evil-forward-sentence (count)
  :type exclusive
  "Move to the next COUNT-th beginning of a sentence or end of a paragraph."
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
  :type exclusive
  "Move to the previous COUNT-th beginning of a sentence or paragraph."
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
  (evil-move-end count 'forward-paragraph 'backward-paragraph))

(evil-define-motion evil-backward-paragraph (count)
  "Move to the beginning of the COUNT-th previous paragraph."
  :jump t
  :type exclusive
  (evil-move-beginning (- (or count 1))
                       'forward-paragraph 'backward-paragraph))

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
  "Move before the next COUNT'th occurence of CHAR."
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
  "Move before the previous COUNT'th occurence of CHAR."
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

(evil-define-motion evil-lookup ()
  "Look up the keyword at point.
Calls `evil-lookup-func'."
  (funcall evil-lookup-func))

(defmacro evil-define-text-object (object args &rest body)
  "Define a text object command OBJECT.
BODY should return a range (BEG END) to the right of point
if COUNT is positive, and to the left of it if negative.

\(fn OBJECT (COUNT) DOC [[KEY VALUE]...] BODY...)"
  (declare (indent defun)
           (debug (&define name lambda-list
                           [&optional stringp]
                           [&rest keywordp sexp]
                           def-body)))
  (let* ((args (delq '&optional args))
         (count (or (pop args) 'count))
         (args (when args `(&optional ,@args)))
         (extend t)
         arg doc key keys type)
    ;; collect docstring
    (when (stringp (car-safe body))
      (setq doc (pop body)))
    ;; collect keywords
    (while (keywordp (car-safe body))
      (setq key (pop body)
            arg (pop body))
      (cond
       ((eq key :type)
        (setq type arg))
       ((eq key :extend-selection)
        (setq extend arg))
       (t
        (setq keys (append keys (list key arg))))))
    ;; macro expansion
    `(evil-define-motion ,object (,count ,@args)
       ,@(when doc `(,doc))
       ,@keys
       :type ,type
       (setq ,count (or ,count 1))
       (when (/= ,count 0)
         (let* ((dir evil-visual-direction)
                (type (or ',type evil-visual-char))
                mark point range region selection temp)
           (cond
            ((and (evil-visual-state-p)
                  (evil-called-interactively-p))
             ;; if we are at the beginning of the Visual selection,
             ;; go to the left (negative COUNT); if at the end,
             ;; go to the right (positive COUNT)
             (setq dir evil-visual-direction
                   ,count (* ,count dir)
                   region (evil-range (mark t) (point))
                   selection (evil-visual-range))
             ;; expand Visual selection so that point
             ;; is outside already selected text
             (when ',extend
               (setq range (evil-range (point) (point) type)))
             (evil-visual-make-selection (mark t) (point) type)
             (evil-visual-expand-region)
             (setq selection (evil-visual-range))
             ;; the preceding selection should contain
             ;; at least one object; if not, add it now
             (let ((,count (- dir)))
               (setq temp (progn ,@body)))
             (when (and (evil-range-p temp)
                        (not (evil-subrange-p temp selection))
                        (or (not ',extend)
                            (if (< dir 0)
                                (= (evil-range-beginning temp)
                                   (evil-range-beginning selection))
                              (= (evil-range-end temp)
                                 (evil-range-end selection)))))
               ;; found an unselected preceding object:
               ;; decrease COUNT and adjust selection boundaries
               (setq ,count (if (< ,count 0) (1+ ,count) (1- ,count)))
               (if ',extend
                   (setq range (evil-range-union temp range))
                 (setq range temp)
                 (evil-set-type range (evil-type range type))))
             (when (/= ,count 0)
               ;; main attempt: find range from current position
               (setq temp (progn ,@body))
               (when (evil-range-p temp)
                 (if ',extend
                     (setq range (evil-range-union temp range))
                   (setq range temp)
                   (evil-set-type range (evil-type range type)))))
             (evil-visual-contract-region)
             (cond
              ((and ',extend (evil-subrange-p range selection))
               ;; Visual fall-back: enlarge selection by one character
               (if (< ,count 0)
                   (evil-visual-select (1- evil-visual-beginning)
                                       evil-visual-end type)
                 (evil-visual-select evil-visual-beginning
                                     (1+ evil-visual-end) type)))
              ((evil-range-p range)
               ;; Find the union of the range and the selection.
               ;; Actually, this uses the region (point and mark)
               ;; rather than the selection to prevent the object
               ;; from unnecessarily overwriting the position of
               ;; the mark at the other end of the selection.
               (setq range (evil-contract-range range))
               (when ',extend
                 (setq range (evil-range-union range region)))
               ;; the beginning is mark and the end is point
               ;; unless the selection goes the other way
               (setq mark  (evil-range-beginning range)
                     point (evil-range-end range)
                     type  (evil-type range type))
               (when (< dir 0)
                 (evil-swap mark point))
               ;; select the range
               (evil-visual-make-selection mark point type))))
            (t
             (setq range (progn ,@body))
             (unless (evil-range-p range)
               (let ((,count (- ,count)))
                 (setq range (progn ,@body))))
             (when (evil-range-p range)
               (setq selection (evil-range (point) (point) type))
               (if ',extend
                   (setq range (evil-range-union range selection))
                 (evil-set-type range (evil-type range type)))
               ;; ensure the range is properly expanded
               (evil-contract-range range)
               (evil-expand-range range)
               (evil-set-range-properties range nil)
               range))))))))

(defun evil-inner-object-range (count forward &optional backward type)
  "Return an inner text object range (BEG END) of COUNT objects.
If COUNT is positive, return objects following point;
if COUNT is negative, return objects preceding point.
FORWARD is a function which moves to the end of an object, and
BACKWARD is a function which moves to the beginning.
If one is unspecified, the other is used with a negative argument."
  (let* ((count (or count 1))
         (forward-func forward)
         (backward-func backward)
         (forward  (or forward
                       (lambda (count)
                         (funcall backward-func (- count)))))
         (backward (or backward
                       (lambda (count)
                         (funcall forward-func (- count)))))
         beg end)
    (when (< count 0)
      (evil-swap forward backward)
      (setq count (abs count)))
    (setq beg (save-excursion
                (funcall forward 1)
                (funcall backward 1)
                (point))
          end (save-excursion
                (funcall forward count)
                (point)))
    (evil-range beg end type)))

(defun evil-an-object-range (count forward &optional backward type newlines)
  "Return a text object range (BEG END) of COUNT objects with whitespace.
See `evil-inner-object-range' for more details."
  (let ((range (evil-inner-object-range count forward backward type)))
    (save-excursion
      (save-restriction
        (if newlines
            (evil-add-whitespace-to-range range count)
          (narrow-to-region
           (save-excursion
             (goto-char (evil-range-beginning range))
             (line-beginning-position))
           (save-excursion
             (goto-char (evil-range-end range))
             (line-end-position)))
          (evil-add-whitespace-to-range range count))))))

(defun evil-paren-range (count open close &optional exclusive)
  "Return a range (BEG END) of COUNT delimited text objects.
OPEN is an opening character and CLOSE is a closing character.
If EXCLUSIVE is non-nil, OPEN and CLOSE are excluded from
the range; otherwise they are included.

This function uses Emacs' syntax table and can therefore only
handle single-character delimiters. To match whole strings,
use `evil-regexp-range'."
  (let ((open-regexp (regexp-quote (string open)))
        (close-regexp (regexp-quote (string close)))
        (count (or count 1))
        level beg end range)
    (save-excursion
      (if (or (evil-in-comment-p)
              (and (evil-in-string-p) (not (eq open close))))
          ;; if in a comment, first look inside the comment only;
          ;; failing that, look outside it
          (or (evil-regexp-range count open-regexp close-regexp exclusive)
              (progn
                (evil-goto-min (evil-string-beginning)
                               (evil-comment-beginning))
                (evil-paren-range count open close exclusive)))
        (with-syntax-table (copy-syntax-table (syntax-table))
          (cond
           ((= count 0))
           ;; if OPEN is equal to CLOSE, handle as string delimiters
           ((eq open close)
            (modify-syntax-entry open "\"")
            (while (not (or (eobp) (evil-in-string-p)))
              (forward-char))
            (when (evil-in-string-p)
              (setq range (evil-range
                           (if exclusive
                               (1+ (evil-string-beginning))
                             (evil-string-beginning))
                           (if exclusive
                               (1- (evil-string-end))
                             (evil-string-end))))))
           (t
            ;; otherwise handle as open and close parentheses
            (modify-syntax-entry open (format "(%c" close))
            (modify-syntax-entry close (format ")%c" open))
            ;; handle edge cases
            (if (< count 0)
                (when (if exclusive
                          (or (looking-back open-regexp)
                              (and (looking-back close-regexp)
                                   (not (looking-at close-regexp))))
                        (looking-back close-regexp))
                  (backward-char))
              (when (if exclusive
                        (or (looking-at close-regexp)
                            (and (looking-at open-regexp)
                                 (not (looking-back open-regexp))))
                      (looking-at open-regexp))
                (forward-char)))
            ;; find OPEN
            (evil-motion-loop (nil count level)
              (condition-case nil
                  (while (progn
                           (backward-up-list 1)
                           (not (looking-at open-regexp))))
                (error nil)))
            (when (/= level count)
              (setq beg (if exclusive (1+ (point)) (point)))
              ;; find CLOSE
              (forward-sexp)
              (setq end (if exclusive (1- (point)) (point)))
              (setq range (evil-range beg end))
              (when exclusive
                (evil-adjust-whitespace-inside-range
                 range (not (eq evil-this-operator 'evil-delete)))))))
          range)))))

;; This simpler, but more general function can be used when
;; `evil-paren-range' is insufficient. Note that as it doesn't use
;; the syntax table, it is unaware of escaped characters (unless
;; such a check is built into the regular expressions).
(defun evil-regexp-range (count open close &optional exclusive)
  "Return a range (BEG END) of COUNT delimited text objects.
OPEN is a regular expression matching the opening sequence,
and CLOSE is a regular expression matching the closing sequence.
If EXCLUSIVE is non-nil, OPEN and CLOSE are excluded from
the range; otherwise they are included. See also `evil-paren-range'."
  (evil-with-or-without-comment
    (let ((either (format "\\(%s\\)\\|\\(%s\\)" open close))
          (count (or count 1))
          (level 0)
          beg end range)
      (save-excursion
        (save-match-data
          ;; find beginning of range: handle edge cases
          (unless (or (looking-at either)
                      (looking-back either nil t))
            ;; Is point inside a delimiter?
            (if (< count 0)
                (when (re-search-backward either nil t)
                  (goto-char (match-end 0))
                  (re-search-forward either nil t))
              (when (re-search-forward either nil t)
                (goto-char (match-beginning 0))
                (re-search-backward either nil t))))
          ;; Is point next to a delimiter?
          (if (< count 0)
              (when (if exclusive
                        (or (looking-back open)
                            (and (looking-back close)
                                 (not (looking-at close))))
                      (looking-back close))
                (goto-char (match-beginning 0)))
            (when (if exclusive
                      (or (looking-at close)
                          (and (looking-at open)
                               (not (looking-back open))))
                    (looking-at open))
              (goto-char (match-end 0))))
          ;; now loop over remainder
          (while (and (< level (abs count))
                      (re-search-backward either nil t))
            (if (looking-at open)
                (setq level (1+ level))
              ;; found a CLOSE, so need to find another OPEN first
              (setq level (1- level))))
          ;; find end of range
          (when (> level 0)
            (forward-char)
            (setq level 1
                  beg (if exclusive
                          (match-end 0)
                        (match-beginning 0)))
            (while (and (> level 0)
                        (re-search-forward either nil t))
              (if (looking-back close)
                  (setq level (1- level))
                ;; found an OPEN, so need to find another CLOSE first
                (setq level (1+ level))))
            (when (= level 0)
              (setq end (if exclusive
                            (match-beginning 0)
                          (match-end 0)))
              (setq range (evil-range beg end))))
          range)))))

(defun evil-xml-range (&optional count exclusive)
  "Return a range (BEG END) of COUNT matching XML tags.
If EXCLUSIVE is non-nil, the tags themselves are excluded
from the range."
  (evil-regexp-range
   count "<\\(?:[^/ ]\\(?:[^>]*?[^/>]\\)?\\)?>" "</[^>]+?>" exclusive))

(defun evil-add-whitespace-to-range (range &optional dir pos regexp)
  "Add whitespace at one side of RANGE, depending on POS.
If POS is before the range, add trailing whitespace;
if POS is after the range, add leading whitespace.
If there is no trailing whitespace, add leading and vice versa.
If POS is inside the range, add trailing if DIR is positive and
leading if DIR is negative. POS defaults to point.
REGEXP is a regular expression for matching whitespace;
the default is \"[ \\f\\t\\n\\r\\v]+\"."
  (let* ((pos (or pos (point)))
         (dir (or (when (<= pos (evil-range-beginning range)) 1)
                  (when (>= pos (evil-range-end range)) -1)
                  dir 1))
         (regexp (or regexp "[ \f\t\n\r\v]+")))
    (save-excursion
      (save-match-data
        (goto-char pos)
        (cond
         ((if (< dir 0) (looking-back regexp) (not (looking-at regexp)))
          (or (evil-add-whitespace-after-range range regexp)
              (evil-add-whitespace-before-range range regexp)))
         (t
          (or (evil-add-whitespace-before-range range regexp)
              (evil-add-whitespace-after-range range regexp))))
        range))))

(defun evil-add-whitespace-before-range (range &optional regexp)
  "Add whitespace at the beginning of RANGE.
REGEXP is a regular expression for matching whitespace;
the default is \"[ \\f\\t\\n\\r\\v]+\".
Returns t if RANGE was successfully increased and nil otherwise."
  (let ((orig (evil-copy-range range))
        (regexp (or regexp "[ \f\t\n\r\v]+")))
    (save-excursion
      (save-match-data
        (goto-char (evil-range-beginning range))
        (when (looking-back regexp nil t)
          ;; exclude the newline on the preceding line
          (goto-char (match-beginning 0))
          (when (eolp) (forward-char))
          (evil-set-range range (point)))
        (not (evil-subrange-p range orig))))))

(defun evil-add-whitespace-after-range (range &optional regexp)
  "Add whitespace at the end of RANGE.
REGEXP is a regular expression for matching whitespace;
the default is \"[ \\f\\t\\n\\r\\v]+\".
Returns t if RANGE was successfully increased and nil otherwise."
  (let ((orig (evil-copy-range range))
        (regexp (or regexp "[ \f\t\n\r\v]+")))
    (save-excursion
      (save-match-data
        (goto-char (evil-range-end range))
        (when (looking-at regexp)
          (evil-set-range range nil (match-end 0)))
        (not (evil-subrange-p range orig))))))

(defun evil-adjust-whitespace-inside-range (range &optional shrink regexp)
  "Adjust whitespace inside RANGE.
Leading whitespace at the end of the line is excluded.
If SHRINK is non-nil, indentation may also be excluded,
and the trailing whitespace is adjusted as well.
REGEXP is a regular expression for matching whitespace;
the default is \"[ \\f\\t\\n\\r\\v]*\".
Returns t if RANGE was successfully adjusted and nil otherwise."
  (let ((orig (evil-copy-range range))
        (regexp (or regexp "[ \f\t\n\r\v]*")))
    (save-excursion
      (goto-char (evil-range-beginning range))
      (when (looking-at (concat regexp "$"))
        (forward-line)
        (if (and shrink evil-auto-indent)
            (back-to-indentation)
          (beginning-of-line))
        (evil-set-range range (point) nil))
      (goto-char (evil-range-end range))
      (when (and shrink (looking-back (concat "^" regexp)))
        (evil-set-range range nil (line-end-position 0)))
      (not (evil-subrange-p orig range)))))

(evil-define-text-object evil-a-word (count &optional bigword)
  "Select a word.
If BIGWORD is non-nil, select a WORD."
  (evil-an-object-range count (if bigword
                                  'evil-move-WORD
                                'evil-move-word)))

(evil-define-text-object evil-inner-word (count &optional bigword)
  "Select inner word.
If BIGWORD is non-nil, select inner WORD."
  (evil-inner-object-range count (if bigword
                                     'evil-move-WORD
                                   'evil-move-word)))

(evil-define-text-object evil-a-WORD (count)
  "Select a WORD."
  (evil-a-word count t))

(evil-define-text-object evil-inner-WORD (count)
  "Select inner WORD."
  (evil-inner-word count t))

(evil-define-text-object evil-a-sentence (count)
  "Select a sentence."
  (evil-an-object-range count 'evil-move-sentence nil nil t))

(evil-define-text-object evil-inner-sentence (count)
  "Select inner sentence."
  (evil-inner-object-range count 'evil-move-sentence))

(evil-define-text-object evil-a-paragraph (count)
  "Select a paragraph."
  :type line
  (evil-an-object-range count 'evil-move-paragraph nil nil t))

(evil-define-text-object evil-inner-paragraph (count)
  "Select inner paragraph."
  :type line
  (evil-inner-object-range count 'evil-move-paragraph))

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
  :extend-selection nil
  (evil-paren-range count ?' ?'))

(evil-define-text-object evil-inner-single-quote (count)
  "Select inner single-quoted expression."
  :extend-selection nil
  (evil-paren-range count ?' ?' t))

(evil-define-text-object evil-a-double-quote (count)
  "Select a double-quoted expression."
  :extend-selection nil
  (evil-paren-range count ?\" ?\"))

(evil-define-text-object evil-inner-double-quote (count)
  "Select inner double-quoted expression."
  :extend-selection nil
  (evil-paren-range count ?\" ?\" t))

(evil-define-text-object evil-a-back-quote (count)
  "Select a back-quoted expression."
  :extend-selection nil
  (evil-paren-range count ?\` ?\`))

(evil-define-text-object evil-inner-back-quote (count)
  "Select inner back-quoted expression."
  :extend-selection nil
  (evil-paren-range count ?\` ?\` t))

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
    (setq this-command 'evil-a-tag)
    (evil-a-tag count))
   (t
    (evil-xml-range count t))))

(provide 'evil-motions)

;;; evil-motions.el ends here
