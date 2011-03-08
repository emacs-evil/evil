;;;; Insert state

(require 'evil-states)
(require 'evil-repeat)

(evil-define-state insert
  "Insert state."
  :tag " <I> "
  (if evil-state
      (progn
        (evil-setup-insert-repeat)
        (add-hook 'evil-insert-state-exit-hook
                  'evil-cleanup-insert-state))
    (remove-hook 'evil-insert-state-exit-hook
                 'evil-cleanup-insert-state)))


(defun evil-cleanup-insert-state ()
  "This function is called when insert-state is about being exited.
This handles the repeat-count of the insert command."
  (evil-teardown-insert-repeat)
  (dotimes (i (1- evil-insert-count))
    (when evil-insert-lines
      (evil-insert-newline-below))
    (evil-execute-repeat-info evil-insert-repeat-info))
  (unless (bolp) (backward-char)))

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

(defun evil-insert-before (count)
  "Switches to insert-state just before point.
The insertion will be repeated `count' times."
  (interactive "p")
  (setq evil-insert-count count
        evil-insert-lines nil)
  (evil-insert-state 1))

(defun evil-insert-after (count)
  "Switches to insert-state just after point.
The insertion will be repeated `count' times."
  (interactive "p")
  (unless (eolp) (forward-char))
  (evil-insert-before count))

(defun evil-insert-above (count)
  "Inserts a new line above point and switches to insert mode.
The insertion will be repeated `count' times."
  (interactive "p")
  (evil-insert-newline-above)
  (setq evil-insert-count count
        evil-insert-lines t)
  (evil-insert-state 1))

(defun evil-insert-below (count)
  "Inserts a new line below point and switches to insert mode.
The insertion will be repeated `count' times."
  (interactive "p")
  (evil-insert-newline-below)
  (setq evil-insert-count count
        evil-insert-lines t)
  (evil-insert-state 1))


(provide 'evil-insert)

;;; evil-insert.el ends here
