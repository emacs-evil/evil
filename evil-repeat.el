;;; Functions for the repeat system

(require 'evil-vars)

(defun evil-normal-pre-repeat ()
  "Called from `pre-command-hook' in vi-state. Initializes
recording of repeat-information for the current command."
  (setq evil-command-changed-buffer nil))

(defun evil-normal-change-repeat (beg end len)
  "Called from `after-change-functions' in vi-state. Records that
the current command is an editing command, i.e., it modified the
buffer."
  (setq evil-command-changed-buffer t))

(defun evil-normal-post-repeat ()
  "Called from `post-command-hook' in vi-state. Finishes
recording of repeat-information and eventually stores it in the
global variable `evil-repeat-info' if the command is repeatable."
  (when (and (functionp this-command)
             evil-command-changed-buffer)
    (setq evil-repeat-info (this-command-keys))))

(defun evil-setup-normal-repeat ()
  "Initializes recording of repeat-information in vi-state."
  (add-hook 'pre-command-hook 'evil-normal-pre-repeat nil t)
  (add-hook 'after-change-functions 'evil-normal-change-repeat nil t)
  (add-hook 'post-command-hook 'evil-normal-post-repeat nil t))

(defun evil-teardown-normal-repeat ()
  "Stops recording of repeat-information in vi-state."
  (remove-hook 'pre-command-hook 'evil-normal-pre-repeat t)
  (remove-hook 'after-change-functions 'evil-normal-change-repeat t)
  (remove-hook 'post-command-hook 'evil-normal-post-repeat t))


;;(defun evil-insert-pre-repeat ()
;;  )

;;(defun evil-insert-change-repeat (beg end len)
;;  )

(defun evil-insert-post-repeat ()
  "Called from `post-command-hook' in insert-state. Finishes
recording of repeat-information and appends it to the global
variable `evil-insert-repeat-info'."
  (when (functionp this-command)
    ;; we ignore keyboard-macros
    (push (this-command-keys) evil-insert-repeat-info)))

(defun evil-setup-insert-repeat ()
  "Initializes recording of repeat-information in insert-state."
  ;(add-hook 'pre-command-hook 'evil-insert-pre-repeat nil t)
  ;(add-hook 'after-change-functions 'evil-insert-change-repeat nil t)
  (add-hook 'post-command-hook 'evil-insert-post-repeat nil t)
  (setq evil-insert-repeat-info nil))

(defun evil-teardown-insert-repeat ()
  "Stops recording of repeat-information in insert-state. The
repeat-information collected during insert-state is merged with
the repeat-information of the commands that entered and left
insert-mode."
  ;(remove-hook 'pre-command-hook 'evil-insert-pre-repeat t)
  ;(remove-hook 'after-change-functions 'evil-insert-change-repeat t)
  (remove-hook 'post-command-hook 'evil-insert-post-repeat t)
  ;; do not forget to add the command that finished insert-mode, usually
  ;; [escape]
  (setq evil-repeat-info
        (apply #'vconcat ;evil-repeat-info
               (reverse (cons (this-command-keys)
                              evil-insert-repeat-info)))))

(provide 'evil-repeat)

;;; evil-repeat.el ends here
