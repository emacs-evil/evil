;;;; Replace state

(require 'evil-core)
(require 'evil-repeat)
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
    (remove-hook 'pre-command-hook 'evil-replace-pre-command t)
    (when evil-move-cursor-back
      (evil-adjust))))
  (setq evil-replace-alist nil))

(defun evil-replace-pre-command ()
  "Remember the character under point."
  (when (evil-replace-state-p)
    (unless (assq (point) evil-replace-alist)
      (add-to-list 'evil-replace-alist
                   (cons (point)
                         (unless (eolp)
                           (char-after)))))))

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
  :motion evil-forward-char
  (interactive "<R>"
               (evil-save-cursor
                 (evil-refresh-cursor 'replace)
                 (list (evil-read-key))))
  (when char
    (if (eq type 'block)
        (save-excursion
          (evil-apply-on-block 'evil-replace beg end nil char))
      (goto-char beg)
      (while (< (point) end)
        (if (eq (char-after) ?\n)
            (forward-char)
          (delete-char 1)
          (if (eq char ?\n)
              (newline)
            (insert-char char 1))))
      (if (eq char ?\n)
          (when evil-auto-indent
            (indent-according-to-mode))
        (goto-char (max beg (1- end)))))))

(provide 'evil-replace)

;;; evil-replace.el ends here
