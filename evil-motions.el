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

(provide 'evil-motions)

;;; evil-motions.el ends here
