;;;; Repeat system

;; A repeat begins when leaving Normal state; it ends when re-entering
;; Normal state. The diagram below shows possible routes between
;; Normal state (N), Insert state (I), Visual state (V),
;; Operator-Pending state (O) and Replace state (R). (Emacs state
;; is an exception: nothing is repeated in that state.)
;;                              ___
;;                             /   \
;;                             | R |
;;                             \___/
;;                             ^   |
;;                             |   |
;;               ___           |___V           ___
;;              /   \ <------- /   \ -------> /   \
;;              | V |          | N |          | O |
;;              \___/ -------> \___/ <------- \___/
;;                  |          |   ^          |
;;                  |          |   |          |
;;                  |          V___|          |
;;                  |          /   \          |
;;                  +--------> | I | <--------+
;;                             \___/
;;
;; Normally, a repeat is triggered whenever a command leaves Normal
;; state, or when it changes the buffer in Normal state (thereby
;; running the `after-change-functions' hook). A command may also
;; trigger a repeat with the :repeat command property. When executed
;; in Normal state, a command with :repeat t will always be repeated;
;; a command with :repeat nil will never be repeated. (Command
;; properties may be set with `evil-add-command-properties'.)
;;
;; When a repeat is being recorded, each command is stored in
;; `evil-repeat-info' from `post-command-hook'. When the repeat ends,
;; the accumulated changes in `evil-repeat-info' are inserted into
;; `evil-repeat-ring'. The dot command, "." (`evil-repeat'),
;; replays the most recent entry in the ring.
;;
;; In most cases, a command is recorded as the key-presses that
;; invoked it. In special cases, it may be recorded as a buffer
;; change. A repeat is represented as a list where each element
;; is either
;;
;;     - an array, which corresponds to a key-sequence, or
;;     - a list (FUNCTION PARAMS...), which will be called as
;;       (apply FUNCTION PARAMS).
;;
;; This information is executed with `evil-execute-repeat-info',
;; which passes key-sequence elements to `execute-kbd-macro' and
;; executes other elements as defined above.
;;
;; It is possible to define special repeation recording functions for
;; specific commands. For this, the :repeat property of the command
;; should be set to the name of the repeation function to be called.
;; The repeation function is called with one argument which is either
;; 'pre or 'post. The function is called twice, once before the
;; command is executed and once after the command has been executed
;; from pre- and post-command-hooks. The function should use, e.g.,
;; `evil-repeat-record-change' or `evil-repeat-record' to append new
;; repeat information.
;;
;; A special version is `evil-execute-repeat-info-with-count'.
;; This function works as `evil-execute-repeat-info', but replaces
;; the count of the first command. This is done by parsing the
;; key-sequence, ignoring all calls to `digit-prefix-argument' and
;; `negative-argument', and prepending the count as a string to the
;; vector of the remaining key-sequence.

(require 'evil-undo)
(require 'evil-states)

(defun evil-repeat-start ()
  "Start recording a new repeat into `evil-repeat-info'."
  (evil-repeat-reset t))

(defun evil-repeat-stop ()
  "Stop recording a repeat.
Update `evil-repeat-ring' with the accumulated changes
in `evil-repeat-info' and clear variables."
  (unwind-protect
      (when (eq evil-recording-repeat t)
        (setq evil-repeat-info
              (evil-normalize-repeat-info evil-repeat-info))
        (when (and evil-repeat-info evil-repeat-ring)
          (ring-insert evil-repeat-ring evil-repeat-info)))
    (evil-repeat-reset nil)))

(defun evil-repeat-reset (flag)
  "Clear all repeat recording variables.
Set `evil-recording-repeat' to FLAG."
  (setq evil-recording-repeat flag
        evil-repeat-info nil
        evil-repeat-buffer nil)
  (evil-repeat-record-buffer))

(defun evil-repeat-record-position (&optional pos)
  "Set `evil-repeat-pos' to POS or point."
  (setq evil-repeat-pos (or pos (point))))

(defun evil-repeat-record-buffer ()
  "Set `evil-repeat-buffer' to the current buffer."
  (unless (minibufferp)
    (setq evil-repeat-buffer (current-buffer))))

(defmacro evil-save-repeat-info (&rest body)
  "Execute BODY, protecting the values of repeat variables."
  (declare (indent defun)
           (debug t))
  `(let (evil-repeat-ring
         evil-recording-repeat
         evil-repeat-info
         evil-repeat-changes
         evil-repeat-pos
         evil-repeat-keys
         evil-repeat-buffer
         this-command
         last-command)
     ,@body))

(defun evil-repeat-different-buffer-p (&optional strict)
  "Whether the buffer has changed in a repeat.
If STRICT is non-nil, returns t if the previous buffer
is unknown; otherwise returns t only if the previous
buffer is known and different from the current buffer."
  (and (or (buffer-live-p evil-repeat-buffer) strict)
       (not (minibufferp))
       (not (eq (current-buffer) evil-repeat-buffer))))

(defun evil-repeat-type (command &optional default)
  "Return the :repeat property of COMMAND.
If COMMAND doesn't have this property, return DEFAULT."
  (let ((type (if (evil-has-property command :repeat)
                  (evil-get-command-property command :repeat)
                default)))
    (or (cdr-safe (assq type evil-repeat-types)) type)))

(defun evil-repeat-record (info)
  "Add INFO to the end of `evil-repeat-info'."
  (when evil-recording-repeat
    (setq evil-repeat-info (nconc evil-repeat-info (list info)))))

;; called from `evil-normal-state-exit-hook'
(defun evil-repeat-start-hook ()
  "Record a new repeat when exiting Normal state.
Does not record in Emacs state or if the current command
has :repeat nil."
  (when (and (eq (evil-repeat-type this-command t) t)
             (not (evil-emacs-state-p)))
    (evil-repeat-start)))

;; called from `pre-command-hook'
(defun evil-repeat-pre-hook ()
  "Prepare the current command for recording the repeation."
  (when (and (functionp this-command)
             (or evil-local-mode (minibufferp)))
    (let ((repeat-type (evil-repeat-type this-command t)))
      (cond
       ;; ignore those commands completely
       ((memq repeat-type '(nil ignore)))
       ;; abort the repeat if the buffer changes, if in
       ;; Emacs state or the command specifies :repeat abort
       ((or (evil-repeat-different-buffer-p)
            (evil-emacs-state-p)
            (eq repeat-type 'abort))
        (evil-repeat-reset 'abort))
       ;; call repeation function for the current command.
       (t
        ;; In normal-state, each command is a single repeation,
        ;; therefore start a new repeation.
        (when (evil-normal-state-p)
          (evil-repeat-start))
        (funcall repeat-type 'pre))))))

;; called from `post-command-hook'
(defun evil-repeat-post-hook ()
  "Finish recording of repeat-information for the current-command."
  (when (and (functionp this-command)
             (or evil-local-mode (minibufferp)))
    (let ((repeat-type (evil-repeat-type this-command t)))
      (cond
       ;; ignore if command should not be recorded.
       ((or (memq repeat-type '(nil ignore abort))
            (evil-emacs-state-p)))
       (t
        (evil-repeat-record-command)
        ;; In normal state, the repeat sequence is complete, so record it.
        (when (evil-normal-state-p)
          (evil-repeat-stop)))))))

(defun evil-repeat-record-command (&optional repeat-type)
  "Calls the post-repeat-information of the current command."
  ;; finish repeation of current command
  (let ((repeat-type (or repeat-type
                         (evil-repeat-type this-command t))))
    (funcall repeat-type 'post)))

(defun evil-repeat-keystrokes (flag)
  "Repeation recording function for commands that are repeated by keystrokes."
  (cond
   ((eq flag 'pre)
    (setq evil-repeat-keys (this-command-keys)))
   ((eq flag 'post)
    (evil-repeat-record (if (zerop (length (this-command-keys)))
                            evil-repeat-keys
                          (this-command-keys)))
    ;; erase commands keys to prevent double recording
    (clear-this-command-keys t))))

(defun evil-repeat-motion (flag)
  "Repeation for motions. Motions are recorded by keystroke but only in insert state."
  (when (memq evil-state '(insert replace))
    (evil-repeat-keystrokes flag)))

(defun evil-repeat-changes (flag)
  "Repeation recording function for commands that are repeated by buffer changes."
  (cond
   ((eq flag 'pre)
    (add-hook 'after-change-functions #'evil-repeat-change-hook nil t)
    (evil-repeat-start-record-changes))
   ((eq flag 'post)
    (remove-hook 'after-change-functions #'evil-repeat-change-hook t)
    (evil-repeat-finish-record-changes))))

;; called from the `after-change-functions' hook
(defun evil-repeat-change-hook (beg end length)
  "Record change information for current command."
  (let ((repeat-type (evil-repeat-type this-command t)))
    (when (and (memq evil-recording-repeat '(t nil))
               (eq repeat-type 'evil-repeat-changes)
               (not (evil-emacs-state-p))
               (not (evil-repeat-different-buffer-p t))
               evil-state)
      (unless evil-recording-repeat
        (evil-repeat-start))
      (evil-repeat-record-change (- beg evil-repeat-pos)
                                 (buffer-substring beg end)
                                 length))))

(defun evil-repeat-record-change (relpos ins ndel)
  "Record the current buffer changes during a repeat.
If CHANGE is specified, it is added to `evil-repeat-changes'."
  (when evil-recording-repeat
    (setq evil-repeat-changes
          (nconc evil-repeat-changes (list (list relpos ins ndel))))))

(defun evil-repeat-start-record-changes ()
  "Starts the recording of a new set of buffer changes."
  (setq evil-repeat-changes nil)
  (evil-repeat-record-position))

(defun evil-repeat-finish-record-changes ()
  "Finishes the recording of buffer changes and records them as repeat."
  (when evil-recording-repeat
    (evil-repeat-record `(evil-execute-change
                          ,evil-repeat-changes
                          ,(- (point) evil-repeat-pos)))
    (setq evil-repeat-changes nil)))

(defun evil-normalize-repeat-info (repeat-info)
  "Concatenate consecutive arrays in REPEAT-INFO.
Returns a single array."
  (let* ((result (cons nil nil))
         (result-last result)
         cur cur-last)
    (dolist (rep repeat-info)
      (cond
       ((null rep))
       ((arrayp rep)
        (setq rep (listify-key-sequence rep))
        (cond
         (cur
          (setcdr cur-last (cons rep nil))
          (setq cur-last (cdr cur-last)))
         (t
          (setq cur (cons rep nil))
          (setq cur-last cur))))
       (t
        (when cur
          (setcdr result-last (cons (apply #'vconcat cur) nil))
          (setq result-last (cdr result-last))
          (setq cur nil))
        (setcdr result-last (cons rep nil))
        (setq result-last (cdr result-last)))))
    (when cur
      (setcdr result-last (cons (apply #'vconcat cur) nil)))
    (cdr result)))

(defun evil-execute-change (changes rel-point)
  "Executes as list of changes.

CHANGES is a list of triples (REL-BEG INSERT-TEXT NDEL).
REL-BEG is the relative position (to point) where the change
takes place. INSERT-TEXT is the text to be inserted at that
position and NDEL the number of characters to be deleted at that
position before insertion.

REL-POINT is the relative position to point before the changed
where point should be placed after all changes."
  (evil-save-repeat-info
    (let ((point (point)))
      (dolist (change changes)
        (goto-char (+ point (nth 0 change)))
        (delete-char (nth 2 change))
        (insert (nth 1 change)))
      (goto-char (+ point rel-point)))))

(defun evil-execute-repeat-info (repeat-info)
  "Executes a repeat-information REPEAT-INFO."
  (evil-save-repeat-info
    (dolist (rep repeat-info)
      (cond
       ((or (arrayp rep) (stringp rep))
        (execute-kbd-macro rep))
       ((consp rep)
        (apply (car rep) (cdr rep)))
       (t
        (error "Unexpected repeat-info: %S" rep))))))

;; TODO: currently we prepend the replacing count before the
;; key-sequence that calls the command. Can we use direct
;; modification of prefix-arg instead? Does it work in
;; conjunction with `execute-kbd-macro'?
(defun evil-execute-repeat-info-with-count (count repeat-info)
  "Repeat the repeat-information REPEAT-INFO with the count of
the first command replaced by COUNT. The count is replaced if
and only if COUNT is non-nil."
  (evil-save-repeat-info
    (cond
     ;; do nothing (zero repeating)
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
     (t
      (evil-execute-repeat-info repeat-info)))))

(evil-define-command evil-repeat (count &optional save-point)
  "Repeat the last editing command with count replaced by COUNT.
If SAVE-POINT is non-nil, do not move point."
  :repeat ignore
  (interactive (list current-prefix-arg
                     (not evil-repeat-move-cursor)))
  (cond
   ((null evil-repeat-ring)
    (error "Already executing repeat"))
   (save-point
    (save-excursion
      (evil-repeat count)))
   (t
    (let ((confirm-kill-emacs t)
          (kill-buffer-hook
           (cons #'(lambda ()
                     (error "Cannot delete buffer in repeat command"))
                 kill-buffer-hook)))
      (evil-with-single-undo
        (setq evil-last-repeat (list (point) count))
        (evil-execute-repeat-info-with-count
         count (ring-ref evil-repeat-ring 0)))))))

;; TODO: the same issue concering disabled undos as for `evil-paste-pop'
(defun evil-repeat-pop (count &optional save-point)
  "Replace the just repeated command with a previously executed command.
Only allowed after `evil-repeat', `evil-repeat-pop' or
`evil-repeat-pop-next'. Uses the same repeat count that
was used for the first repeat.

The COUNT argument inserts the COUNT-th previous kill.
If COUNT is negative, this is a more recent kill."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     (not evil-repeat-move-cursor)))
  (cond
   ((not (and (eq last-command 'evil-repeat)
              evil-last-repeat))
    (error "Previous command was not evil-repeat: %s" last-command))
   (save-point
    (save-excursion
      (evil-repeat-pop count)))
   (t
    (evil-undo-pop)
    (goto-char (car evil-last-repeat))
    ;; rotate the repeat-ring
    (while (> count 0)
      (when evil-repeat-ring
        (ring-insert-at-beginning evil-repeat-ring
                                  (ring-remove evil-repeat-ring 0)))
      (setq count (1- count)))
    (setq this-command 'evil-repeat)
    (evil-repeat (cadr evil-last-repeat)))))

(defun evil-repeat-pop-next (count &optional save-point)
  "Same as `evil-repeat-pop', but with negative COUNT."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     (not evil-repeat-move-cursor)))
  (evil-repeat-pop (- count) save-point))

(defadvice read-key-sequence (before evil activate)
  "Record `this-command-keys' before it is reset."
  (evil-repeat-record-command))

(provide 'evil-repeat)

;;; evil-repeat.el ends here
