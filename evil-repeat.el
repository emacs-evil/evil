;;; Functions for the repeat system

(require 'evil-vars)

(defun evil-normal-pre-repeat ()
  "Called from `pre-command-hook' in vi-state. Initializes
recording of repeat-information for the current command."
  (setq evil-command-modified-buffer nil))

(defun evil-normal-change-repeat (beg end len)
  "Called from `after-change-functions' in vi-state. Records that
the current command is an editing command, i.e., it modified the
buffer."
  (setq evil-command-modified-buffer t))

(defun evil-normal-post-repeat ()
  "Called from `post-command-hook' in vi-state. Finishes
recording of repeat-information and eventually stores it in the
global variable `evil-repeat-info' if the command is repeatable."
  (when (and (functionp this-command)
             evil-command-modified-buffer)
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

  ;; Note that this will automatically add the key-sequence
  ;; that just activated insert-mode to `evil-insert-repeat-info',
  ;; because this post-command-hook is run for the current command.
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
        (apply #'vconcat (reverse (cons (this-command-keys)
                                        evil-insert-repeat-info)))))


(defun evil-replace-command-prefix-arg ()
  "Replaces the replace-count by `evil-repeat-count'.
Called from a pre-command-hook during repeation."
  (unless (memq this-command '(digit-argument negative-argument))
    (remove-hook 'pre-command-hook 'evil-replace-command-prefix-arg)
    (setq prefix-arg evil-repeat-count)))


;; TODO: repeating will not work for operator because we do not have
;;       operator-pending-state, yet. In this case a non-nil value of
;;       evil-repeat-count must be used as count for the motion.
(defun evil-execute-repeat-info (count repeat-info)
  "Repeat the repeat-information `repeat-info' with the count of
the first command replaced by `count'. The count is replaced if
and only if `count' is non-nil."
  (let ((evil-repeating-command t))
    (if count
	(let ((evil-repeat-count count))
	  (add-hook 'pre-command-hook 'evil-replace-command-prefix-arg)
	  (execute-kbd-macro repeat-info)
	  (remove-hook 'pre-command-hook 'evil-replace-command-prefix-arg))
      (execute-kbd-macro repeat-info))))
  
(defun evil-repeat (count)
  "Repeat the last editing command with count replaced by `count'."
  (interactive "P")
  (evil-execute-repeat-info count evil-repeat-info))

(provide 'evil-repeat)

;;; evil-repeat.el ends here
