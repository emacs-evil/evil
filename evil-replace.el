;;;; Replace state

(require 'evil-vars)
(require 'evil-common)
(require 'evil-states)
(require 'evil-operators)

(evil-define-state replace
  "Replace state."
  :tag " <R> "
  :cursor hbar
  :message "-- REPLACE --"
  (cond
   ((evil-replace-state-p)
    (overwrite-mode 1)
    (add-hook 'pre-command-hook 'evil-replace-pre-command nil t))
   (t
    (overwrite-mode -1)
    (remove-hook 'pre-command-hook 'evil-replace-pre-command t)))
  (setq evil-replace-alist nil))

(defun evil-replace-pre-command ()
  "Remember the character under point."
  (when (evil-replace-state-p)
    (unless (assq (point) evil-replace-alist)
      (add-to-list 'evil-replace-alist
                   (cons (point)
                         (unless (eolp)
                           (char-after)))
                   t))))

(defun evil-replace-backspace ()
  "Restore character under cursor."
  (interactive)
  (let (char)
    (backward-char)
    (when (assq (point) evil-replace-alist)
      (setq char (cdr (assq (point) evil-replace-alist)))
      (save-excursion
        (delete-char 1)
        (when char
          (insert char))))))

(evil-define-operator evil-replace (beg end type char)
  "Replace text from BEG to END with CHAR."
  :move-point t ; TODO: remove
  :motion evil-forward-char
  (interactive (list (evil-save-cursor
                       (evil-set-cursor evil-replace-state-cursor)
                       ;; TODO: this doesn't handle special input
                       ;; methods such as "C-x 8 ."
                       (read-char))))
  (let ((opoint (point))) ; `save-excursion' doesn't work reliably
    (goto-char beg)
    (unwind-protect
        (if (eq type 'block)
            (evil-apply-on-block 'evil-replace beg end nil char)
          (while (< (point) end)
            (if (eq (char-after) ?\n)
                (forward-char)
              (delete-char 1)
              (insert-char char 1))))
      (goto-char opoint))))

(provide 'evil-replace)

;;; evil-replace.el ends here
