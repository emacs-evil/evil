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
ARGS is the argument list, which must contain
at least one argument: the count."
  (declare (indent defun)
           (debug (&define name lambda-list
                           [&optional stringp]
                           [&rest keywordp sexp]
                           def-body)))
  (let (count interactive keyword type)
    ;; collect COUNT
    (setq args (delq '&optional args)
          count (or (pop args) 'count))
    ;; collect docstring
    (when (stringp (car body))
      (setq doc (pop body)))
    ;; collect keywords
    (while (keywordp (setq keyword (car body)))
      (setq body (cdr body))
      (cond
       ((eq keyword :type)
        (setq type (pop body)))
       (t
        (pop body))))
    ;; collect `interactive' specification
    (when (eq (car-safe (car-safe body)) 'interactive)
      (setq interactive (cdr (pop body))))
    ;; macro expansion
    `(progn
       (add-to-list 'evil-motions ',motion t)
       (when ',type
         (evil-set-type ',motion ',type))
       (defun ,motion (&optional ,count ,@args)
         ,@(when doc `(,doc))
         (interactive
          (append (list (prefix-numeric-value
                         current-prefix-arg))
                  ,@interactive))
         (setq ,count (or ,count 1))
         ,@body))))

(defmacro evil-narrow-to-line (&rest body)
  "Narrows to the current line."
  `(save-restriction
     (narrow-to-region
      (line-beginning-position)
      (if (evil-visual-state-p)
          (line-end-position)
        (max (line-beginning-position)
             (1- (line-end-position)))))
     (let ((p (point)))
       (condition-case err
           (progn ,@body)
         (error
          (when (= p (point))
            (signal (car err) (cdr err))))))))

(evil-define-motion evil-forward-char (count)
  "Move cursor to the right by COUNT characters."
  :type exclusive
  (evil-narrow-to-line (forward-char count)))

(evil-define-motion evil-backward-char (count)
  "Move cursor to the left by COUNT characters."
  :type exclusive
  (evil-narrow-to-line (backward-char count)))

(evil-define-motion evil-previous-line (count)
  "Move the cursor COUNT lines up."
  :type line
  (let (line-move-visual)
    (with-no-warnings
      (previous-line count))))

(evil-define-motion evil-next-line (count)
  "Move the cursor COUNT lines down."
  :type line
  (let (line-move-visual)
    (with-no-warnings
      (next-line count))))

;; This motion can be used for repeated commands like 'dd'
;;(evil-define-motion vim:motion-lines (linewise count)
;;  "Moves count - 1 lines down."
;;  (let (line-move-visual)
;;    (next-line (1- (or count 1)))))

(evil-define-motion evil-next-visual-line (count)
  "Move the cursor COUNT screen lines up."
  :type exclusive
  (let ((line-move-visual t))
    (with-no-warnings
      (previous-line count))))

(evil-define-motion evil-previous-visual-line (count)
  "Move the cursor COUNT screen lines down."
  :type exclusive
  (let ((line-move-visual t))
    (with-no-warnings
      (next-line count))))

(evil-define-motion evil-move-to-window-line (count)
  "Moves the cursor to line COUNT from the top of the window on
the first non-blank character."
  :type line
  (move-to-window-line (or count 0))
  (back-to-indentation))

(evil-define-motion evil-move-to-middle-window-line (count)
  "Moves the cursor to the middle line of the current window on
the first non-blank character."
  :type line
  (move-to-window-line (/ (window-body-height) 2))
  (back-to-indentation))

(evil-define-motion evil-move-to-last-window-line (count)
  "Moves the cursor to line COUNT from the bottom of the window on
the first non-blank character."
  :type line
  (move-to-window-line (- (window-body-height) (or count 0) 1))
  (back-to-indentation))

(evil-define-motion evil-beginning-of-line ()
  "Move the cursor to the beginning of the current line."
  :type exclusive
  (beginning-of-line))

(evil-define-motion evil-beginning-of-line-or-digit-argument ()
  "Move the cursor to the beginning of the current line.
This function passes its command to `digit-argument' (usually a
0) if it is not the first event."
  :type exclusive
  (call-interactively (if current-prefix-arg
                          'digit-argument
                        'evil-beginning-of-line)))

(evil-define-motion evil-first-non-blank (count)
  "Move the cursor to the first non-blank character of the
current line."
  :type exclusive
  (back-to-indentation))

(evil-define-motion evil-end-of-line (count)
  "Move the cursor to the end of the current line. If COUNT is
given move COUNT - 1 lines downward first."
  :type inclusive
  (end-of-line count)
  (unless (or (evil-visual-state-p)
              (bolp))
    (backward-char)))

(evil-define-motion evil-last-non-blank (count)
  "Move the cursor to the last non-blank charactor of the current
line. If COUNT is given move COUNT - 1 lines downward first."
  :type inclusive
  (goto-char
   (save-excursion
     (beginning-of-line count)
     (if (re-search-forward "[ \t]*$")
         (max (line-beginning-position)
              (1- (match-beginning 0)))
       (line-beginning-position)))))

(evil-define-motion evil-move-to-first-non-blank-beg (count)
  "Moves the cursor to the first non-blank charactor of line
COUNT, default the first line."
  :type line
  (if count
      (goto-line count)
    (goto-char (point-min)))
  (evil-first-non-blank 1))

(evil-define-motion evil-move-to-first-non-blank-end (count)
  "Moves the cursor to the first non-blank charactor of line
COUNT, default the last line."
  :type line
  (if count
      (goto-line count)
    (goto-char (point-max)))
  (evil-first-non-blank 1))

(evil-define-motion evil-beginning-of-visual-line (count)
  "Move the cursor to the first character of the current screen
line."
  :type exclusive
  (beginning-of-visual-line))

(evil-define-motion evil-first-non-blank-of-visual-line (count)
  "Move the cursor to the first non blank character of the
current screen line."
  :type exclusive
  (evil-beginning-of-visual-line 1)
  (skip-chars-forward " \t\r"))

(evil-define-motion evil-end-of-visual-line (count)
  "Move the cursor to the last character of the current screen
line. If COUNT is given move COUNT - 1 screen lines downward
first."
  :type inclusive
  (end-of-visual-line count)
  (unless (or (evil-visual-state-p)
              (bolp))
    (backward-char)))

(provide 'evil-motions)

;;; evil-motions.el ends here
