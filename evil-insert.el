;;;; Insert state

(require 'evil-states)
(require 'evil-digraphs)

(evil-define-state insert
  "Insert state."
  :tag " <I> "
  :cursor (bar . 2)
  :message "-- INSERT --"
  :exit-hook (evil-cleanup-insert-state)
  (cond
   ((evil-insert-state-p)
    (evil-setup-insert-repeat)
    (unless evil-want-fine-undo
      (evil-start-undo-step)))
   (t
    (unless evil-want-fine-undo
      (evil-end-undo-step))
    (when evil-move-cursor-back
      (evil-adjust)))))

(defun evil-cleanup-insert-state ()
  "Called when Insert state is about to be exited.
Handles the repeat-count of the insertion command."
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
  "Switch to Insert state just before point.
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
  "Switch to Insert state just after point.
The insertion will be repeated COUNT times."
  (interactive "p")
  (unless (eolp) (forward-char))
  (evil-insert-before count vcount))

(defun evil-insert-above (count)
  "Insert a new line above point and switch to Insert state.
The insertion will be repeated COUNT times."
  (interactive "p")
  (evil-insert-newline-above)
  (setq evil-insert-count count
        evil-insert-lines t
        evil-insert-vcount nil)
  (when evil-auto-indent
    (indent-according-to-mode))
  (evil-insert-state 1))

(defun evil-insert-below (count)
  "Insert a new line below point and switch to Insert state.
The insertion will be repeated COUNT times."
  (interactive "p")
  (evil-insert-newline-below)
  (setq evil-insert-count count
        evil-insert-lines t
        evil-insert-vcount nil)
  (when evil-auto-indent
    (indent-according-to-mode))
  (evil-insert-state 1))

(defun evil-insert-beginning-of-line (count &optional vcount)
  "Switch to Insert state just before the first non-blank character
on the current line. The insertion will be repeated COUNT times."
  (interactive "p")
  (if evil-auto-indent
      (back-to-indentation)
    (beginning-of-line))
  (setq evil-insert-count count
        evil-insert-lines nil
        evil-insert-vcount (and vcount
                                (> vcount 1)
                                (list (line-number-at-pos)
                                      #'evil-first-non-blank
                                      vcount)))
  (evil-insert-state 1))

(defun evil-insert-end-of-line (count &optional vcount)
  "Switch to Insert state at the end of the current line.
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
           (setq char1 (read-key))
           (setq string (string char1))
           (put-text-property 0 1 'face 'minibuffer-prompt string)
           (put-text-property 0 1 'cursor t string)
           (overlay-put overlay 'after-string string)
           (setq char2 (read-key)))
       (delete-overlay overlay))
     (list count (list char1 char2))))
  (let ((digraph (or (evil-digraph digraph)
                     ;; use the last character if undefined
                     (cadr digraph))))
    (dotimes (var count)
      (insert digraph))))

(provide 'evil-insert)

;;; evil-insert.el ends here
