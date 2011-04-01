;;;; Insert state

(require 'evil-states)
(require 'evil-repeat)
(require 'evil-motions)

(evil-define-state insert
  "Insert state."
  :tag " <I> "
  :cursor (bar . 2)
  :message "-- INSERT --"
  (cond
   ((evil-insert-state-p)
    (evil-setup-insert-repeat)
    (add-hook 'evil-insert-state-exit-hook
              'evil-cleanup-insert-state))
   (t
    (when evil-move-cursor-back
      (unless (bolp) (backward-char)))
    (remove-hook 'evil-insert-state-exit-hook
                 'evil-cleanup-insert-state))))

(defun evil-cleanup-insert-state ()
  "This function is called when insert-state is about being exited.
This handles the repeat-count of the insert command."
  (evil-teardown-insert-repeat)
  (dotimes (i (1- evil-insert-count))
    (when evil-insert-lines
      (evil-insert-newline-below))
    (evil-execute-repeat-info evil-insert-repeat-info))
  (when evil-insert-vcount
    (let ((line (nth 0 evil-insert-vcount))
          (col (nth 1 evil-insert-vcount))
          (vcount (nth 2 evil-insert-vcount)))
      (save-excursion
        (dotimes (v (1- vcount))
          (goto-char (point-min))
          (forward-line (+ line v))
          (if (numberp col)
              (move-to-column col t)
            (funcall col))
          (dotimes (i (or evil-insert-count 1))
            (evil-execute-repeat-info evil-insert-repeat-info)))))))

(defun evil-insert-newline-above ()
  "Inserts a new line above point and places point in that line
w.r.t. indentation."
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (back-to-indentation))

(defun evil-insert-newline-below ()
  "Inserts a new line below point and places point in that line
w.r.t. indentation."
  (end-of-line)
  (newline)
  (back-to-indentation))

(defun evil-insert-before (count &optional vcount)
  "Switches to insert-state just before point.
The insertion will be repeated COUNT times and repeated once for
the next VCOUNT-1 lines starting at the same column."
  (interactive "p")
  (setq evil-insert-count count
        evil-insert-lines nil
        evil-insert-vcount (and vcount
                                (> vcount 1)
                                (list (line-number-at-pos)
                                      (current-column)
                                      vcount)))
  (evil-insert-state 1))

(defun evil-insert-after (count &optional vcount)
  "Switches to insert-state just after point.
The insertion will be repeated COUNT times."
  (interactive "p")
  (unless (eolp) (forward-char))
  (evil-insert-before count vcount))

(defun evil-insert-above (count)
  "Inserts a new line above point and switches to insert mode.
The insertion will be repeated COUNT times."
  (interactive "p")
  (evil-insert-newline-above)
  (setq evil-insert-count count
        evil-insert-lines t
        evil-insert-vcount nil)
  (indent-according-to-mode)
  (evil-insert-state 1))

(defun evil-insert-below (count)
  "Inserts a new line below point and switches to insert mode.
The insertion will be repeated COUNT times."
  (interactive "p")
  (evil-insert-newline-below)
  (setq evil-insert-count count
        evil-insert-lines t
        evil-insert-vcount nil)
  (indent-according-to-mode)
  (evil-insert-state 1))

(defun evil-insert-beginning-of-line (count &optional vcount)
  "Switches to insert-state just before the first non-blank character on the current line.
The insertion will be repeated COUNT times."
  (interactive "p")
  (evil-first-non-blank)
  (setq evil-insert-count count
        evil-insert-lines nil
        evil-insert-vcount (and vcount
                                (> vcount 1)
                                (list (line-number-at-pos)
                                      #'evil-first-non-blank
                                      vcount)))
  (evil-insert-state 1))

(defun evil-insert-end-of-line (count &optional vcount)
  "Switches to insert-state at the end of the current line.
The insertion will be repeated COUNT times."
  (interactive "p")
  (end-of-line)
  (setq evil-insert-count count
        evil-insert-lines nil
        evil-insert-vcount (and vcount
                                (> vcount 1)
                                (list (line-number-at-pos)
                                      #'end-of-line
                                      vcount)))
  (evil-insert-state 1))

(provide 'evil-insert)

;;; evil-insert.el ends here
