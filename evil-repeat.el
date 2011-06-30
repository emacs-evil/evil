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
  (unless evil-recording-repeat
    (evil-repeat-reset t)
    (evil-repeat-record-position)
    (evil-repeat-record-buffer)))

(defun evil-repeat-stop ()
  "Stop recording a repeat.
Update `evil-repeat-ring' with the accumulated changes
in `evil-repeat-info' and clear variables."
  (unwind-protect
      (progn
        (setq evil-repeat-info
              (evil-normalize-repeat-info evil-repeat-info))
        (when evil-repeat-info
          (ring-insert evil-repeat-ring evil-repeat-info)))
    (evil-repeat-reset)))

(defun evil-repeat-reset (&optional flag)
  "Clear all repeat recording variables.
Set `evil-recording-repeat' to FLAG."
  (when (markerp evil-repeat-marker)
    (set-marker evil-repeat-marker nil))
  (setq evil-repeat-marker nil
        evil-recording-repeat flag
        evil-repeat-info nil
        evil-repeat-changes nil
        evil-repeat-keys nil
        evil-repeat-buffer nil))

(defun evil-repeat-type (command &optional default)
  "Return the :repeat property of COMMAND.
If COMMAND doesn't have this property, return DEFAULT."
  (if (evil-has-property command :repeat)
      (evil-get-command-property command :repeat)
    default))

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
  "Record new repeat if the current command has :repeat t.
Disallow repeat if the command specifies :repeat nil."
  (cond
   ;; abort the repeat if the buffer changes, if in
   ;; Emacs state or the command specifies :repeat abort
   ((or (evil-repeat-different-buffer-p)
        (evil-emacs-state-p)
        (eq (evil-repeat-type this-command) 'abort))
    (evil-repeat-reset 'abort))
   ;; Already in repeat?
   (evil-recording-repeat
    (when (eq evil-recording-repeat 'abort)
      (evil-repeat-reset)))
   ;; Start a repeat from Normal state?
   ((evil-normal-state-p)
    (cond
     ;; :repeat t, start repeat
     ((eq (evil-repeat-type this-command) t)
      (evil-repeat-start))
     ;; :repeat nil, prevent repeat
     ((eq (evil-repeat-type this-command t) nil)
      (evil-repeat-reset 'abort))
     ;; no :repeat, but the command may change the buffer
     (t
      (evil-repeat-record-buffer)))))
  ;; refresh current repeat
  (when evil-recording-repeat
    (evil-repeat-record-keys)
    (evil-repeat-record-position)
    (evil-repeat-record-buffer)
    (setq evil-repeat-changes nil)))

;; called from `post-command-hook'
(defun evil-repeat-post-hook ()
  "Refresh `evil-repeat-info' while recording a repeat."
  (cond
   ((not evil-recording-repeat))
   ;; abort the repeat
   ((or (eq evil-recording-repeat 'abort)
        (evil-repeat-different-buffer-p)
        (evil-emacs-state-p))
    (evil-repeat-reset))
   ;; finish the repeat
   ((evil-normal-state-p)
    (evil-repeat-record-command)
    (evil-repeat-stop))
   (t
    ;; refresh the repeat
    (when evil-recording-repeat
      (evil-repeat-record-command)
      (evil-repeat-record-position)))))

;; called from the `after-change-functions' hook
(defun evil-repeat-change-hook (beg end length)
  "Record change information for current command."
  (unless (or (eq evil-recording-repeat 'abort)
              (evil-repeat-different-buffer-p t)
              (null (evil-repeat-type this-command t))
              (evil-emacs-state-p)
              (null evil-state))
    (unless evil-recording-repeat
      (evil-repeat-start))
    (when (and (eq (evil-repeat-type this-command) 'change)
               evil-repeat-marker)
      (evil-repeat-record-change
       (list (- beg evil-repeat-marker)
             (buffer-substring beg end)
             length)))))

(defun evil-repeat-record-command ()
  "Record the current command into `evil-repeat-info'."
  (unless evil-recording-repeat
    (evil-repeat-start))
  (cond
   ((not (functionp this-command))) ; ignore macros
   ;; prefix arguments always preceed the actual commands and they
   ;; are part of the key sequence of the actual command, therefore
   ;; they can be safely ignored
   ((memq this-command
          '(digit-argument
            negative-argument
            universal-argument
            universal-argument-minus
            universal-argument-other-key)))
   ;; check if the command is change-based
   ((eq (evil-repeat-type this-command) 'change)
    (evil-repeat-record `(evil-execute-change
                          ,evil-repeat-changes
                          ,(- (point) evil-repeat-marker))))
   ;; usual command: record by key-sequence
   (t
    (evil-repeat-record (if (> (length (this-command-keys)) 0)
                            (this-command-keys)
                          evil-repeat-keys)))))

(defun evil-repeat-record (info)
  "Add INFO to the end of `evil-repeat-info'."
  (unless evil-recording-repeat
    (evil-repeat-start))
  (setq evil-repeat-info (nconc evil-repeat-info (list info))))

(defun evil-repeat-record-change (change)
  "Add CHANGE to the end of `evil-repeat-changes'."
  (unless evil-recording-repeat
    (evil-repeat-start))
  (setq evil-repeat-changes (nconc evil-repeat-changes (list change))))

(defun evil-repeat-record-position (&optional pos)
  "Set `evil-repeat-marker' to POS or point."
  (unless evil-recording-repeat
    (evil-repeat-start))
  (unless (markerp evil-repeat-marker)
    (setq evil-repeat-marker (make-marker)))
  (set-marker evil-repeat-marker (or pos (point))))

(defun evil-repeat-record-buffer ()
  "Set `evil-repeat-buffer' to the current buffer."
  (unless evil-recording-repeat
    (evil-repeat-start))
  (setq evil-repeat-buffer (current-buffer)))

;; Some functions, such as `execute-kbd-macro', may irrevocably
;; clear `this-command-keys'. Therefore, make a backup from
;; `pre-command-hook'.
(defun evil-repeat-record-keys (&optional pos)
  "Set `evil-repeat-keys' to the current value of `this-command-keys'."
  (unless evil-recording-repeat
    (evil-repeat-start))
  (setq evil-repeat-keys (this-command-keys)))

(defmacro evil-save-repeat-info (&rest body)
  "Execute BODY, protecting the values of repeat variables."
  (declare (debug t)
           (indent defun))
  `(let ((evil-repeat-ring (ring-copy evil-repeat-ring))
         evil-repeating-command
         evil-recording-repeat
         evil-repeat-info
         evil-repeat-changes
         evil-repeat-marker
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
       (not (eq (current-buffer) evil-repeat-buffer))))

(defun evil-normalize-repeat-info (repeat-info)
  "Concatenate consecutive arrays in REPEAT-INFO.
Returns a single array."
  (let* ((result (cons nil nil))
         (result-last result)
         cur cur-last)
    (dolist (rep repeat-info)
      (cond
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
    (let ((p (point)))
      (dolist (change changes)
        (goto-char (+ p (nth 0 change)))
        (delete-char (nth 2 change))
        (insert (nth 1 change)))
      (goto-char (+ p rel-point)))))

(defun evil-execute-repeat-info (repeat-info)
  "Executes a repeat-information REPEAT-INFO."
  (let ((evil-repeating-command t))
    (evil-save-repeat-info
      (dolist (rep (copy-sequence repeat-info))
        (cond
         ((arrayp rep)
          (execute-kbd-macro rep))
         ((consp rep)
          (apply (car rep) (cdr rep)))
         (t
          (error "Unexpected repeat-info: %S" rep)))))))

;; TODO: currently we prepend the replacing count before the
;; key-sequence that calls the command. Can we use direct
;; modification of prefix-arg instead? Does it work in
;; conjunction with execute-kbd-macro?
(defun evil-execute-repeat-info-with-count (count repeat-info)
  "Repeat the repeat-information REPEAT-INFO with the count of
the first command replaced by COUNT. The count is replaced if
and only if COUNT is non-nil."
  (let ((evil-repeating-command t))
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
        (evil-execute-repeat-info repeat-info))))))

(evil-define-command evil-repeat (count &optional save-point)
  "Repeat the last editing command with count replaced by COUNT.
If SAVE-POINT is non-nil, do not move point."
  :repeat nil
  (interactive (list current-prefix-arg
                     (not evil-repeat-move-cursor)))
  (cond
   (evil-repeating-command
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
      (setq evil-last-repeat (list (point) count))
      (evil-save-repeat-info
        (evil-with-undo
          (evil-execute-repeat-info-with-count
           count (ring-ref evil-repeat-ring 0))))))))

;; TODO: the same issue concering disabled undos as for `evil-paste-pop'
(defun evil-repeat-pop (count)
  "Replace the just repeated command with a previously executed command.
This command is allowed only immediately after a `evil-repeat',
`evil-repeat-pop' or `evil-repeat-pop-next'. This command uses
the same repeat count that was used for the first repeat.

The COUNT argument inserts the COUNT-th previous kill. If COUNT
is negative this is a more recent kill."
  (interactive "p")
  (unless (and (eq last-command 'evil-repeat)
               evil-last-repeat)
    (error "Previous command was not evil-repeat: %s" last-command))
  (evil-undo-pop)
  (goto-char (car evil-last-repeat))
  ;; rotate the repeat-ring
  (while (> count 0)
    (ring-insert-at-beginning evil-repeat-ring
                              (ring-remove evil-repeat-ring 0))
    (setq count (1- count)))
  (setq this-command 'evil-repeat)
  (evil-repeat (cadr evil-last-repeat)))

(defun evil-repeat-pop-next (count)
  "Same as `evil-repeat-pop' with negative COUNT."
  (interactive "p")
  (evil-repeat-pop (- count)))

;; updates `evil-repeat-info' properly
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
                       (make-char-table 'display-table
                                        'self-insert-command) t)
          (use-global-map new-global-map)
          (evil-repeat-record (this-command-keys))
          (aref (read-key-sequence prompt nil t) 0))
      (use-global-map old-global-map))))

(provide 'evil-repeat)

;;; evil-repeat.el ends here
