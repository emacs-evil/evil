;;;; Functions for the repeat system

;; The repeat system is based on generalized keyboard macros. A repeat
;; information is a list where each element is
;; - an array, which corresponds to a key-sequence
;; - a list (FUNCTION PARAMS...) which will be called via
;;   (apply FUNCTION PARAMS)
;;
;; The repeat information is stored in the variable
;; `evil-repeat-info'. The repeat-information has to be created state
;; specific usually using pre-/post-command-hook and
;; after-change-function hooks.
;;
;; In normal-state after-change-function hook is used to check whether
;; the current command changed the buffer. If this happend the
;; corresponding key-sequence (this-command-events) is recorded as
;; repeat-information.
;;
;; Insert-state accumulates repeat-information for each command
;; executed. Most commands are recorded by the key-sequence used to
;; call them in the post-command-hook `evil-insert-post-repeat'. Some
;; commands if the corresponding repeat-type (set via
;; `evil-set-insert-repeat-type') is 'change, not the key-sequence but
;; the buffer-change itself is recorded in the after-change-function
;; `evil-insert-change-repeat' which accumulates the buffer-changes
;; and the final `evil-insert-post-repeat' hook inserts an entry
;; (evil-execute-change CHANGES... end-point) to `evil-repeat-info-ring'.
;;
;; Repeat-information can be executed (replayed) via
;; `evil-execute-repeat-info'. This function either calles
;; `execute-kbd-macro' for each key-sequence element of the
;; repeat-info and executes the recorded function for all other
;; elements.
;;
;; A special version is `evil-execute-repeat-info-with-count'. This
;; function works as `evil-execute-repeat-info' replaces the count of
;; the first command. This is done by parsing the key-sequence,
;; ignoring all calls to `digit-prefix-argument' and
;; `negative-argument' and prepending the count as a string to the
;; vector of the remaining key-sequence.

(require 'evil-vars)

(defun evil-add-repeat-info (repeat-info)
  "Adds a repeat-information to `evil-repeat-info-ring'
discarding old commands."
  (ring-insert evil-repeat-info-ring repeat-info))

(defun evil-insert-repeat-type (command)
  "Returns the repeat-type of a certain `command'."
  (gethash command evil-insert-repeat-types))

(defun evil-set-insert-repeat-type (command type)
  "Changes the repeat type of `command' to `type'.
`command' is the symbol of the command `type' is either nil,
'ignore or 'change. A nil value means the command is repeated by
the key-sequence that invoked it. 'ignore means the command
should be ignored completely. 'change means the command is
repeated by tracking the buffer changed."
  (puthash command type evil-insert-repeat-types))

(defun evil-repeat-normal-command-p ()
  "Return non-nil iff the current command should be recored for repeation."
  (and evil-state
       (if (evil-has-properties-p this-command)
           (evil-repeatable-p this-command)
         evil-command-modified-buffer)))

(defun evil-normal-pre-repeat ()
  "Called from `pre-command-hook' in vi-state. Initializes
recording of repeat-information for the current command."
  ;; prefix arguments always preceed the actual commands and they are
  ;; part of the key sequence of the actual command, therefore they
  ;; can be safely ignored
  (setq evil-command-modified-buffer nil
        evil-normal-repeat-info nil))

(defun evil-normal-change-repeat (beg end len)
  "Called from `after-change-functions' in vi-state. Records that
the current command is an editing command, i.e., it modified the
buffer."
  (when evil-state
    (setq evil-command-modified-buffer t)))

(defun evil-normal-post-repeat ()
  "Called from `post-command-hook' in vi-state. Finishes
recording of repeat-information and eventually stores it in the
global variable `evil-repeat-info-ring' if the command is repeatable."
  (when (and (functionp this-command)
             (evil-repeat-normal-command-p))
    (unless (memq this-command '(digit-argument
                                 negative-argument
                                 universal-argument
                                 universal-argument-minus
                                 universal-argument-other-key))
      (evil-add-repeat-info
       (evil-normalize-repeat-info (reverse (cons
                                             (this-command-keys)
                                             evil-normal-repeat-info)))))))

(defun evil-setup-normal-repeat ()
  "Initializes recording of repeat-information in vi-state."
  (setq evil-command-modified-buffer nil)
  (add-hook 'pre-command-hook 'evil-normal-pre-repeat nil t)
  (add-hook 'after-change-functions 'evil-normal-change-repeat nil t)
  (add-hook 'post-command-hook 'evil-normal-post-repeat nil t))

(defun evil-teardown-normal-repeat ()
  "Stops recording of repeat-information in vi-state."
  (remove-hook 'pre-command-hook 'evil-normal-pre-repeat t)
  (remove-hook 'after-change-functions 'evil-normal-change-repeat t)
  (remove-hook 'post-command-hook 'evil-normal-post-repeat t))

(defun evil-insert-pre-repeat ()
  "Called from `pre-command-hook' in insert mode. Decides how the
  current command show be recorded for repeation."
  (when (functionp this-command)
    ;; we ignore keyboard-macros
    (setq evil-insert-repeat-type
          (evil-insert-repeat-type this-command))
    (when (eq evil-insert-repeat-type 'change)
      (setq evil-insert-repeat-point (point)
            evil-insert-repeat-changes nil))))

(defun evil-insert-change-repeat (beg end len)
  "Called from `after-change-functions' in insert mode. When the
current command should be repeated by change the change
information is recorded."
  (when (eq evil-insert-repeat-type 'change)
    (push (list (- beg evil-insert-repeat-point)
                (buffer-substring beg end)
                len)
          evil-insert-repeat-changes)))

(defun evil-insert-post-repeat ()
  "Called from `post-command-hook' in insert-state. Finishes
recording of repeat-information and appends it to the global
variable `evil-insert-repeat-info'."
  (when (functionp this-command)
    (unless (memq this-command '(digit-argument
                                 negative-argument
                                 universal-argument
                                 universal-argument-minus
                                 universal-argument-other-key))
      (cond
       ;; This is the command that enabled insert state.
       ((eq evil-insert-repeat-info 'startup)
        (push (this-command-keys) evil-normal-repeat-info)
        (setq evil-insert-repeat-info nil))
       ;; Check if the command is change-based
       ((eq evil-insert-repeat-type 'change)
        (push (list #'evil-execute-change
                    (reverse evil-insert-repeat-changes)
                    (- (point) evil-insert-repeat-point))
              evil-insert-repeat-info))
       ;; Usual command, record by key-sequence
       (t
        (push (this-command-keys) evil-insert-repeat-info))))))

(defun evil-setup-insert-repeat ()
  "Initializes recording of repeat-information in insert-state."
  (add-hook 'pre-command-hook 'evil-insert-pre-repeat nil t)
  (add-hook 'after-change-functions 'evil-insert-change-repeat nil t)

  ;; Note that this will automatically add the key-sequence
  ;; that just activated insert-mode to `evil-insert-repeat-info',
  ;; because this post-command-hook is run for the current command.
  (add-hook 'post-command-hook 'evil-insert-post-repeat nil t)
  (setq evil-insert-repeat-info 'startup))

(defun evil-teardown-insert-repeat ()
  "Stops recording of repeat-information in insert-state. The
repeat-information collected during insert-state is merged with
the repeat-information of the commands that entered and left
insert-mode."
  (remove-hook 'pre-command-hook 'evil-insert-pre-repeat t)
  (remove-hook 'after-change-functions 'evil-insert-change-repeat t)
  (remove-hook 'post-command-hook 'evil-insert-post-repeat t)
  ;; do not forget to add the command that finished insert-mode, usually
  ;; [escape]
  (setq evil-insert-repeat-info
        (evil-normalize-repeat-info
         (nreverse evil-insert-repeat-info)))
  (ring-insert evil-repeat-info-ring
               (evil-normalize-repeat-info
                (append (reverse evil-normal-repeat-info)
                        evil-insert-repeat-info
                        (list (this-command-keys))))))

(defun evil-normalize-repeat-info (repeat-info)
  "Concatenates consecutive arrays in the repeat-info to a single
array."
  (let* ((result (cons nil nil))
         (result-last result)
         cur
         cur-last)
    (dolist (rep repeat-info)
      (if (arrayp rep)
          (if cur
              (progn
                (setcdr cur-last (cons rep nil))
                (setq cur-last (cdr cur-last)))
            (setq cur (cons rep nil))
            (setq cur-last cur))
        (when cur
          (setcdr result-last (cons (apply #'vconcat cur) nil))
          (setq result-last (cdr result-last))
          (setq cur nil))
        (setcdr result-last (cons rep nil))
        (setq result-last (cdr result-last))))
    (when cur
      (setcdr result-last (cons (apply #'vconcat cur) nil)))
    (cdr result)))

(defun evil-execute-change (changes rel-point)
  "Executes as list of changes.

`changes' is a list of triples (REL-BEG INSERT-TEXT NDEL).
REL-BEG is the relative position (to point) where the change
takes place. INSERT-TEXT is the text to be inserted at that
position and NDEL the number of characters to be deleted at that
position before insertion.

`rel-point' is the relative position to point before the changed
where point should be placed after all changes."
  (let ((p (point)))
    (dolist (change changes)
      (goto-char (+ p (nth 0 change)))
      (delete-char (nth 2 change))
      (insert (nth 1 change)))
    (goto-char (+ p rel-point))))

(defun evil-execute-repeat-info (repeat-info)
  "Executes a repeat-information `repeat-info'."
  (dolist (rep repeat-info)
    (cond
     ((arrayp rep)
      (execute-kbd-macro rep))
     ((consp rep)
      (apply (car rep) (cdr rep)))
     (t
      (error "Unexpected repeat-info: %S" rep)))))

;; TODO: currently we prepend the replacing count before the
;;       key-sequence that calls the command. Can we use direct
;;       modification of prefix-arg instead? Does it work in
;;       conjunction with execute-kbd-macro?
(defun evil-execute-repeat-info-with-count (count repeat-info)
  "Repeat the repeat-information `repeat-info' with the count of
the first command replaced by `count'. The count is replaced if
and only if `count' is non-nil."
  (let ((evil-repeating-command t))
    (cond
     ;; do nothing (zero repeation)
     ((and count (zerop count)))

     ;; replace count
     (count
      (let ((evil-repeat-count count)
            done)
        (while (and repeat-info
                    (arrayp (car repeat-info))
                    (not done))
          (let* ((count-and-cmd (evil-extract-count (pop repeat-info))))
            (push (vconcat (number-to-string count)
                           (nth 2 count-and-cmd)
                           (nth 3 count-and-cmd))
                  repeat-info)
            (setq done t)))
        (evil-execute-repeat-info repeat-info)))

     ;; repeat with original count
     (t (evil-execute-repeat-info repeat-info)))))

(evil-define-command evil-repeat (count)
  "Repeat the last editing command with count replaced by `count'."
  :repeatable nil
  (interactive "P")
  (let ((confirm-kill-emacs t)
        (kill-buffer-hook
         (cons #'(lambda ()
                   (error "Cannot delete buffer in repeat command."))
               kill-buffer-hook))
        (evil-repeat-info-ring (ring-copy evil-repeat-info-ring))
        (this-command this-command)
        (last-command last-command))
    (setq evil-last-repeat (list (point) count))
    (evil-with-undo
      (evil-execute-repeat-info-with-count count (ring-ref evil-repeat-info-ring 0)))))

;; TODO: the same issue concering disabled undos as for `evil-paste-pop'
(defun evil-repeat-pop (count)
  "Replace the just repeated command with a previously executed command.
This command is allowed only immediatly after a `evil-repeat',
`evil-repeat-pop' or `evil-repeat-pop-next'. This command uses
the same repeat count that was used for the first repeat.

The COUNT argument inserts the COUNTth previous kill.  If COUNT
is negative this is a more recent kill."
  (interactive "p")
  (unless (and (eq last-command 'evil-repeat)
               evil-last-repeat)
    (error "Previous command was not evil-repeat: %s" last-command))
  (evil-undo-pop)
  (goto-char (car evil-last-repeat))
  ;; rotate the repeat-ring
  (while (> count 0)
    (ring-insert-at-beginning evil-repeat-info-ring
                              (ring-remove evil-repeat-info-ring 0))
    (setq count (1- count)))
  (setq this-command 'evil-repeat)
  (evil-repeat (cadr evil-last-repeat)))

(defun evil-repeat-pop-next (count)
  "Same as `evil-repeat-pop' with negative COUNT."
  (interactive "p")
  (evil-repeat-pop (- count)))

;; `read-key' is introduced in Emacs 23.2
(defun evil-read-key (&optional prompt)
  "Read a key from the keyboard.
Translates it according to the input method."
  (let ((old-global-map (current-global-map))
        (new-global-map (make-sparse-keymap))
        (overriding-terminal-local-map (make-sparse-keymap))
        overriding-local-map)
    (unwind-protect
        (progn
          (define-key new-global-map [menu-bar]
            (lookup-key global-map [menu-bar]))
          (define-key new-global-map [tool-bar]
            (lookup-key global-map [tool-bar]))
          (add-to-list 'new-global-map
                       (make-char-table 'display-table 'self-insert-command) t)
          (use-global-map new-global-map)
          (push (this-command-keys) evil-normal-repeat-info)
          (aref (read-key-sequence prompt nil t) 0))
      (use-global-map old-global-map))))

(provide 'evil-repeat)

;;; evil-repeat.el ends here
