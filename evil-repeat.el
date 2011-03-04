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


;; TODO: The count argument for repeat is tricky especially for
;;       key-sequences. We will probably have to either parse the
;;       key-sequence for count arguments or do some magic with
;;       rebinding `digit-argument' and friends. The first approach
;;       seems to be the cleaner way but it is difficult to realize
;;       which numbers in the key-sequence have to be replaced,
;;       because they may belong to another keyboard-macro or belong
;;       to commands in insert-state, ... The second approach is
;;       probably easier to get done right, but it must be sensitive
;;       to the current state (only the count of normal-state and
;;       op-pending-state must be changed) and has to modify
;;       `prefix-arg'. Is a pre-command-hook the right place?
(defun evil-repeat (count)
  "Repeat the last editing command with count replaced by `count'."
  (interactive "P")
  (let ((evil-repeating-command t))
    (if count
        (error "`count' for repeat not yet supported.")
      (execute-kbd-macro evil-repeat-info))))


(provide 'evil-repeat)

;;; evil-repeat.el ends here
