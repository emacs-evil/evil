;;;; Common functions and utilities

(require 'evil-vars)
(require 'evil-compatibility)
(require 'evil-interactive)

;;; List functions

(eval-and-compile
  (defun evil-add-to-alist (list-var key val &rest elements)
    "Add the assocation of KEY and VAL to the value of LIST-VAR.
If the list already contains an entry for KEY, update that entry;
otherwise add at the end of the list."
    (let ((tail (symbol-value list-var)))
      (while (and tail (not (equal (car-safe (car-safe tail)) key)))
        (setq tail (cdr tail)))
      (if tail
          (setcar tail (cons key val))
        (set list-var (append (symbol-value list-var)
                              (list (cons key val)))))
      (if elements
          (apply 'evil-add-to-alist list-var elements)
        (symbol-value list-var)))))

;; custom version of `delete-if'
(defun evil-filter-list (predicate list &optional pointer)
  "Delete by side-effect all items satisfying PREDICATE in LIST.
Stop when reaching POINTER. If the first item satisfies PREDICATE,
there is no way to remove it by side-effect; therefore, write
\(setq foo (evil-delete-if 'predicate foo)) to be sure of
changing the value of `foo'."
  (let ((tail list) elt head)
    (while (and tail (not (eq tail pointer)))
      (setq elt (car tail))
      (cond
       ((funcall predicate elt)
        (setq tail (cdr tail))
        (if head
            (setcdr head tail)
          (setq list tail)))
       (t
        (setq head tail
              tail (cdr tail)))))
    list))

(defun evil-member-if (predicate list &optional pointer)
  "Find the first item satisfying PREDICATE in LIST.
Stop when reaching POINTER."
  (let (elt)
    (catch 'done
      (while (and (consp list) (not (eq list pointer)))
        (setq elt (car list))
        (if (funcall predicate elt)
            (throw 'done elt)
          (setq list (cdr list)))))))

(defun evil-concat-lists (&rest sequences)
  "Concatenate lists, removing duplicates."
  (let (result)
    (dolist (sequence sequences)
      (dolist (elt sequence)
        (add-to-list 'result elt nil 'eq)))
    (nreverse result)))

(defun evil-concat-alists (&rest sequences)
  "Concatenate association lists, removing duplicates."
  (let (result)
    (dolist (sequence sequences)
      (dolist (elt sequence)
        (setq result (assq-delete-all (car-safe elt) result))
        (add-to-list 'result elt)))
    (nreverse result)))

(defun evil-concat-plists (&rest sequences)
  "Concatenate property lists, removing duplicates."
  (let (result)
    (dolist (sequence sequences result)
      (while sequence
        (setq result
              (plist-put result (pop sequence) (pop sequence)))))))

(defun evil-get-property (alist key &optional prop)
  "Return property PROP for KEY in ALIST.
ALIST is an association list with entries of the form
\(KEY . PLIST), where PLIST is a property list.
If PROP is nil, return all properties for KEY.
If KEY is t, return an association list of keys
and their PROP values."
  (unless (or (keywordp prop) (null prop))
    (setq prop (intern (format ":%s" prop))))
  (cond
   ((null prop)
    (cdr (assq key alist)))
   ((eq key t)
    (let (result val)
      (dolist (entry alist result)
        (setq key (car entry)
              val (cdr entry))
        (when (plist-member val prop)
          (setq val (plist-get val prop))
          (push (cons key val) result)))))
   (t
    (plist-get (cdr (assq key alist)) prop))))

(defun evil-put-property (alist-var key prop val &rest properties)
  "Set PROP to VAL for KEY in ALIST-VAR.
ALIST-VAR points to an association list with entries of the form
\(KEY . PLIST), where PLIST is a property list storing PROP and VAL."
  (set alist-var
       (let* ((alist (symbol-value alist-var))
              (plist (cdr (assq key alist))))
         (while prop
           (unless (keywordp prop)
             (setq prop (intern (format ":%s" prop))))
           (setq plist (plist-put plist prop val)
                 prop (pop properties)
                 val (pop properties)))
         (setq alist (assq-delete-all key alist))
         (push (cons key plist) alist))))

(defun evil-state-property (state prop)
  "Return property PROP for STATE."
  (evil-get-property evil-state-properties state prop))

(defmacro evil-swap (this that &rest vars)
  "Swap the values of variables THIS and THAT.
If three or more arguments are given, the values are rotated.
E.g., (evil-swap A B C) sets A to B, B to C, and C to A."
  `(progn
     (setq ,this (prog1 ,that
                   (setq ,that ,this)))
     ,@(when vars
         `((evil-swap ,that ,@vars)))))

(defmacro evil-sort (min max &rest vars)
  "Place the smallest value in MIN and the largest in MAX.
If three or more arguments are given, place the smallest
value in the first argument and the largest in the last,
sorting in between."
  (let ((sorted (make-symbol "sortvar")))
    `(let ((,sorted (sort (list ,min ,max ,@vars) '<)))
       (setq ,min (pop ,sorted)
             ,max (pop ,sorted)
             ,@(apply 'append (mapcar (lambda (var)
                                        (list var `(pop ,sorted)))
                                      vars))))))

(defmacro evil-loop (spec &rest body)
  "Loop with countdown variable.
Evaluate BODY with VAR counting down from COUNT to 0.
COUNT can be negative, in which case VAR counts up instead.
The return value is the value of VAR when the loop
terminates, which is 0 if the loop completes successfully.
RESULT specifies a variable for storing this value.

\(fn (VAR COUNT [RESULT]) BODY...)"
  (declare (indent defun)
           (debug dolist))
  (let* ((i (make-symbol "loopvar"))
         (var (pop spec))
         (count (pop spec))
         (result (pop spec)))
    (setq var (or (unless (eq var result) var) i)
          result (or result var))
    `(let ((,var ,count))
       (setq ,result ,var)
       (while (/= ,var 0)
         ,@body
         (if (> ,var 0)
             (setq ,var (1- ,var))
           (setq ,var (1+ ,var)))
         (setq ,result ,var))
       ,var)))

;; toggleable version of `with-temp-message'
(defmacro evil-save-echo-area (&rest body)
  "Save the echo area; execute BODY; restore the echo area.
Intermittent messages are not logged in the *Messages* buffer."
  (declare (indent defun)
           (debug t))
  `(let ((inhibit-quit t)
         evil-echo-area-message
         evil-write-echo-area)
     (unwind-protect
         (progn
           (evil-echo-area-save)
           ,@body)
       (evil-echo-area-restore))))

(defun evil-echo-area-save ()
  "Save the current echo area in `evil-echo-area-message'."
  (setq evil-echo-area-message (current-message)))

(defun evil-echo-area-restore ()
  "Restore the echo area from `evil-echo-area-message'.
Does not restore if `evil-write-echo-area' is non-nil."
  (unless evil-write-echo-area
    (if evil-echo-area-message
        (message "%s" evil-echo-area-message)
      (message nil)))
  (setq evil-echo-area-message nil
        evil-write-echo-area nil))

(defun evil-echo (string &rest args)
  "Display an unlogged message in the echo area.
That is, the message is not logged in the *Messages* buffer.
\(To log the message, just use `message'.)"
  (let (message-log-max)
    (unless evil-locked-display
      (apply 'message string args))))

(defmacro evil-with-locked-display (&rest body)
  "Execute BODY with locked display.
State changes will not change the cursor, refresh the mode line
or display a message in the echo area."
  (declare (indent defun)
           (debug t))
  `(let ((evil-locked-display t))
     ,@body))

(defmacro evil-save-state (&rest body)
  "Save the current state; execute BODY; restore the state."
  (declare (indent defun)
           (debug t))
  `(let* ((evil-state evil-state)
          (evil-previous-state evil-previous-state)
          (evil-next-state evil-next-state)
          (old-state evil-state)
          (inhibit-quit t))
     (unwind-protect
         (progn ,@body)
       (evil-change-state old-state))))

(defmacro evil-with-state (state &rest body)
  "Change to STATE and execute BODY without refreshing the display.
Restore the previous state afterwards."
  (declare (indent defun)
           (debug t))
  `(evil-with-locked-display
     (evil-save-state
       (evil-change-state ',state)
       ,@body)))

(defun evil-refresh-cursor (&optional state buffer)
  "Refresh the cursor for STATE in BUFFER.
STATE defaults to the current state.
BUFFER defaults to the current buffer."
  (let* ((state (or state evil-state 'normal))
         (default (or evil-default-cursor t))
         (cursor (symbol-value (evil-state-property state :cursor)))
         (color (or (and (stringp cursor) cursor)
                    (and (listp cursor)
                         (evil-member-if 'stringp cursor)))))
    (with-current-buffer (or buffer (current-buffer))
      ;; if both STATE and `evil-default-cursor'
      ;; specify a color, don't set it twice
      (when (and color (listp default))
        (setq default (evil-filter-list 'stringp default)))
      (evil-set-cursor default)
      (evil-set-cursor cursor))))

(defun evil-set-cursor (specs)
  "Change the cursor's apperance according to SPECS.
SPECS may be a cursor type as per `cursor-type', a color
string as passed to `set-cursor-color', a zero-argument
function for changing the cursor, or a list of the above."
  (unless (and (listp specs)
               (null (cdr-safe (last specs))))
    (setq specs (list specs)))
  (dolist (spec specs)
    (cond
     ((functionp spec)
      (condition-case nil
          (funcall spec)
        (error nil)))
     ((stringp spec)
      (evil-set-cursor-color spec))
     (t
      (setq cursor-type spec)))))

(defun evil-set-cursor-color (color)
  "Set the cursor color to COLOR."
  (unless (equal (frame-parameter nil 'cursor-color) color)
    ;; `set-cursor-color' forces a redisplay, so only
    ;; call it when the color actually changes
    (set-cursor-color color)))

(defmacro evil-save-cursor (&rest body)
  "Save the current cursor; execute BODY; restore the cursor."
  (declare (indent defun)
           (debug t))
  `(let ((cursor cursor-type)
         (color (frame-parameter (selected-frame) 'cursor-color))
         (inhibit-quit t))
     (unwind-protect
         (progn ,@body)
       (evil-set-cursor cursor)
       (evil-set-cursor color))))

(defun evil-move-to-column (column &optional dir force)
  "Move point to column COLUMN in the current line.
Places point at left of the tab character (at the right if DIR
is non-nil) and returns point."
  (interactive "p")
  (move-to-column column force)
  (unless force
    (when (or (not dir) (and (numberp dir) (< dir 1)))
      (when (> (current-column) column)
        (evil-adjust))))
  (point))

(defun evil-set-keymap-prompt (map prompt)
  "Set the prompt-string of MAP to PROMPT."
  (delq (keymap-prompt map) map)
  (when prompt
    (setcdr map (cons prompt (cdr map)))))

;;; Markers

(defun evil-global-marker-p (char)
  "Whether CHAR denotes a global marker."
  (or (and (>= char ?A) (<= char ?Z))
      (assq char (default-value 'evil-markers-alist))))

(defun evil-set-marker (char &optional pos advance)
  "Set the marker denoted by CHAR to position POS.
POS defaults to the current position of point.
If ADVANCE is t, the marker advances when inserting text at it;
otherwise, it stays behind."
  (interactive (list (read-char)))
  (let ((marker (evil-get-marker char t)) alist)
    (unless (markerp marker)
      (cond
       ((and marker (symbolp marker) (boundp marker))
        (set marker (or (symbol-value marker) (make-marker)))
        (setq marker (symbol-value marker)))
       ((functionp marker)
        (error "Cannot set special marker `%c'" char))
       ((evil-global-marker-p char)
        (setq alist (default-value 'evil-markers-alist)
              marker (make-marker))
        (evil-add-to-alist 'alist char marker)
        (setq-default evil-markers-alist alist))
       (t
        (setq marker (make-marker))
        (evil-add-to-alist 'evil-markers-alist char marker))))
    (add-hook 'kill-buffer-hook 'evil-swap-out-markers nil t)
    (set-marker-insertion-type marker advance)
    (set-marker marker (or pos (point)))))

(defun evil-get-marker (char &optional raw)
  "Return the marker denoted by CHAR.
This is either a marker object as returned by `make-marker',
a number, a cons cell (FILE . POS) with FILE being a string
and POS a number, or nil. If RAW is non-nil, then the
return value may also be a variable, a movement function,
or a marker object pointing nowhere."
  (let ((marker (if (evil-global-marker-p char)
                    (cdr-safe (assq char (default-value
                                           'evil-markers-alist)))
                  (cdr-safe (assq char evil-markers-alist)))))
    (save-excursion
      (if raw
          marker
        (when (and (symbolp marker) (boundp marker))
          (setq marker (symbol-value marker)))
        (when (functionp marker)
          (funcall marker)
          (setq marker (point)))
        (when (markerp marker)
          (if (eq (marker-buffer marker) (current-buffer))
              (setq marker (marker-position marker))
            (setq marker (and (marker-buffer marker) marker))))
        (when (or (numberp marker)
                  (markerp marker)
                  (and (consp marker)
                       (stringp (car marker))
                       (numberp (cdr marker))))
          marker)))))

(defun evil-swap-out-markers ()
  "Turn markers into file references when the buffer is killed."
  (and buffer-file-name
       (dolist (entry evil-markers-alist)
         (and (markerp (cdr entry))
              (eq (marker-buffer (cdr entry)) (current-buffer))
              (setcdr entry (cons buffer-file-name
                                  (marker-position (cdr entry))))))))

(defun evil-set-jump (&optional pos)
  "Set jump point at POS."
  (unless (region-active-p)
    (evil-save-echo-area
      (mapc (lambda (marker)
              (set-marker marker nil))
            evil-jump-list)
      (setq evil-jump-list nil)
      (push-mark pos))))

(defun evil-get-register (register &optional noerror)
  "Return contents of REGISTER.
Signal an error if empty, unless NOERROR is non-nil."
  (when (characterp register)
    (or (cond
         ((eq register ?\")
          (current-kill 0))
         ((and (<= ?0 register) (<= register ?9))
          (current-kill (- register ?0) t))
         ((eq register ?*)
          (let ((x-select-enable-primary t))
            (current-kill 0)))
         ((eq register ?+)
          (let ((x-select-enable-clipboard t))
            (current-kill 0)))
         (t
          (get-register register)))
        (unless noerror
          (error "Register `%c' is empty" register)))))

(defun evil-set-register (register text)
  "Set the contents of register REGISTER to TEXT."
  (cond
   ((eq register ?\")
    (kill-new text))
   ((and (<= ?0 register) (<= register ?9))
    (if (null kill-ring)
        (kill-new text)
      (let ((kill-ring-yank-pointer kill-ring-yank-pointer)
            interprogram-paste-function
            interprogram-cut-function)
        (current-kill (- register ?0))
        (setcar kill-ring-yank-pointer text))))
   ((eq register ?*)
    (let ((x-select-enable-primary t))
      (kill-new text)))
   ((eq register ?+)
    (let ((x-select-enable-clipboard t))
      (kill-new text)))
   (t
    (set-register register text))))

;; custom version of `gensym'
(defun evil-generate-symbol (&optional intern)
  "Return a new uninterned symbol.
If INTERN is non-nil, intern the symbol."
  (setq evil-symbol-counter (1+ evil-symbol-counter))
  (if intern
      (intern (format "evil-symbol-%d" evil-symbol-counter))
    (make-symbol (format "evil-symbol-%d" evil-symbol-counter))))

;;; Key sequences

(defun evil-extract-count (keys)
  "Splits the key-sequence KEYS into prefix-argument and the rest.
Returns the list (PREFIX CMD SEQ REST), where PREFIX is the
prefix count, CMD the command to be executed, SEQ the subsequence
calling CMD, and REST is all remaining events in the
key-sequence. PREFIX and REST may be nil if they do not exist.
If a command is bound to some keyboard macro, it is expanded
recursively."
  (catch 'done
    (let* ((len (length keys))
           (beg 0)
           (end 1)
           (found-prefix nil))
      (while (and (<= end len))
        (let ((cmd (key-binding (substring keys beg end))))
          (cond
           ((memq cmd '(undefined nil))
            (error "No command bound to %s" (substring keys beg end)))
           ((arrayp cmd) ; keyboard macro, replace command with macro
            (setq keys (vconcat (substring keys 0 beg)
                                cmd
                                (substring keys end))
                  end (1+ beg)
                  len (length keys)))
           ((functionp cmd)
            (if (or (memq cmd '(digit-argument negative-argument))
                    (and found-prefix
                         (evil-get-command-property
                          cmd :digit-argument-redirection)))
                ;; skip those commands
                (setq found-prefix t ; found at least one prefix argument
                      beg end
                      end (1+ end))
              ;; a real command, finish
              (throw 'done
                     (list (unless (zerop beg)
                             (string-to-number
                              (concat (substring keys 0 beg))))
                           cmd
                           (substring keys beg end)
                           (when (< end len)
                             (substring keys end))))))
           (t ; append a further event
            (setq end (1+ end))))))
      (error "Key sequence contains no complete binding"))))

(defun evil-mouse-events-p (keys)
  "Returns non-nil iff KEYS contains a mouse event."
  (catch 'done
    (dotimes (i (length keys))
      (when (or (and (fboundp 'mouse-event-p)
                     (mouse-event-p (aref keys i)))
                (mouse-movement-p (aref keys i)))
        (throw 'done t)))
    nil))

;;; Command properties

(defmacro evil-define-command (command &rest body)
  "Define a command COMMAND.

\(fn COMMAND (ARGS...) DOC [[KEY VALUE]...] BODY...)"
  (declare (indent defun)
           (debug (&define name
                           [&optional lambda-list]
                           [&optional stringp]
                           [&rest keywordp sexp]
                           [&optional ("interactive" [&rest form])]
                           def-body)))
  (let ((keys (plist-put nil :repeat t))
        arg args doc doc-form key)
    ;; collect arguments
    (when (listp (car-safe body))
      (setq args (pop body)))
    ;; collect docstring
    (when (> (length body) 1)
      (if (eq (car-safe (car-safe body)) 'format)
          (setq doc-form (pop body))
        (when (stringp (car-safe body))
          (setq doc (pop body)))))
    ;; collect keywords
    (while (keywordp (car-safe body))
      (setq key (pop body)
            arg (pop body))
      (unless nil ; TODO: add keyword check
        (plist-put keys key arg)))
    ;; collect interactive
    (when (and body
               (consp (car body))
               (eq (car (car body)) 'interactive))
      (let* ((interactive (pop body))
             (result (apply #'evil-interactive-form (cdr interactive)))
             (form (car result))
             (attrs (cdr result)))
        (push (list 'interactive form) body)
        ;; The next code is a copy of the previous one but does not
        ;; overwrite properties.
        (while (keywordp (car-safe attrs))
          (setq key (pop attrs)
                arg (pop attrs))
          (unless (or nil ; TODO: add keyword check
                      (plist-member keys key))
            (plist-put keys key arg)))))
    `(progn
       ;; the compiler does not recognize `defun' inside `let'
       ,(when (and command body)
          `(defun ,command ,args
             ,@(when doc `(,doc))
             ,@body))
       ,(when (and command doc-form)
          `(put ',command 'function-documentation ,doc-form))
       ;; set command properties for symbol or lambda function
       (let ((func ',(if (and (null command) body)
                         `(lambda ,args ,@body)
                       command)))
         (apply 'evil-set-command-properties func ',keys)
         func))))

(defun evil-add-command-properties (command &rest properties)
  "Add Evil PROPERTIES to COMMAND.
PROPERTIES should be a list of :keywords and values, e.g.:

    (evil-add-command-properties 'my-command :repeat t)

See also `evil-set-command-properties'."
  (apply 'evil-put-property 'evil-command-properties command properties))

(defun evil-set-command-properties (command &rest properties)
  "Set Evil PROPERTIES of COMMAND.
PROPERTIES should be a list of :keywords and values, e.g.:

    (evil-set-command-properties 'my-command :repeat t)

This erases all previous properties. To only add properties,
use `evil-add-command-properties'."
  (setq evil-command-properties
        (assq-delete-all command evil-command-properties))
  (apply #'evil-add-command-properties command properties))

;; If no evil-properties are defined for the command, several parts of
;; Evil apply certain default rules, e.g., the repeat-system decides
;; whether the command is repeatable by monitoring buffer changes.
(defun evil-has-properties-p (command)
  "Whether Evil properties are defined for COMMAND."
  (evil-get-property evil-command-properties command))

(defun evil-has-property (command property)
  "Whether COMMAND has Evil PROPERTY."
  (plist-member (evil-get-property evil-command-properties command)
                property))

(defun evil-get-command-property (command property)
  "Returns the value of Evil PROPERTY of COMMAND."
  (evil-get-property evil-command-properties command property))

(defmacro evil-redirect-digit-argument (map keys target)
  "Bind a wrapper function calling TARGET or `digit-argument'.
MAP is a keymap for binding KEYS to the wrapper for TARGET.
The wrapper only calls `digit-argument' if a prefix-argument
has already been started; otherwise TARGET is called."
  (let* ((target (eval target))
         (wrapper (intern (format "evil-digit-argument-or-%s"
                                  target))))
    `(progn
       (define-key ,map ,keys ',wrapper)
       (evil-define-command ,wrapper ()
         :digit-argument-redirection ,target
         :keep-visual t
         :repeat nil
         (interactive)
         (cond
          (current-prefix-arg
           (setq this-command 'digit-argument)
           (call-interactively 'digit-argument))
          (t
           (setq this-command ',target)
           (call-interactively ',target)))))))

(defun evil-yank-handler (&optional motion)
  "Return the yank handler for MOTION.
MOTION defaults to the current motion."
  (setq motion (or motion evil-this-motion))
  (evil-get-command-property motion :yank-handler))

(defun evil-declare-motion (command)
  "Declare COMMAND to be a movement function.
This ensures that it behaves correctly in Visual state."
  (evil-add-command-properties command :keep-visual t :repeat 'motion))

(defun evil-declare-repeat (command)
  "Declare COMMAND to be repeatable."
  (evil-add-command-properties command :repeat t))

(defun evil-declare-not-repeat (command)
  "Declare COMMAND to be nonrepeatable."
  (evil-add-command-properties command :repeat nil))

(defun evil-declare-ignore-repeat (command)
  "Declare COMMAND to be nonrepeatable."
  (evil-add-command-properties command :repeat 'ignore))

(defun evil-declare-change-repeat (command)
  "Declare COMMAND to be repeatable by buffer changes."
  (evil-add-command-properties command :repeat 'change))

(defun evil-declare-abort-repeat (command)
  "Declare COMMAND to be nonrepeatable."
  (evil-add-command-properties command :repeat 'abort))

;;; Region

(defun evil-transient-save ()
  "Save Transient Mark mode and make the new setup buffer-local.
The variables to save are listed in `evil-transient-vars'.
Their values are stored in `evil-transient-vals'."
  (dolist (var evil-transient-vars)
    (when (and (boundp var)
               (not (assq var evil-transient-vals)))
      (push (list var (symbol-value var)
                  (and (assq var (buffer-local-variables)) t))
            evil-transient-vals)
      (make-variable-buffer-local var))))

(defun evil-transient-restore ()
  "Restore Transient Mark mode from `evil-transient-vals'."
  (let (entry local var val)
    (while (setq entry (pop evil-transient-vals))
      (setq var (pop entry)
            val (pop entry)
            local (pop entry))
      (unless local
        (kill-local-variable var))
      (unless (equal (symbol-value var) val)
        (if (fboundp var)
            (funcall var (if var 1 -1))
          (setq var val))))))

;; In theory, an active region implies Transient Mark mode, and
;; disabling Transient Mark mode implies deactivating the region.
;; In practice, Emacs never clears `mark-active' except in Transient
;; Mark mode, so we define our own toggle functions to make things
;; more predictable.
(defun evil-transient-mark (&optional arg)
  "Toggle Transient Mark mode.
Ensure that the region is properly deactivated.
Enable with positive ARG, disable with negative ARG."
  (unless (numberp arg)
    (setq arg (if transient-mark-mode -1 1)))
  (cond
   ((< arg 1)
    (evil-active-region -1)
    ;; Transient Mark mode cannot be disabled
    ;; while CUA mode is enabled
    (when (fboundp 'cua-mode)
      (cua-mode -1))
    (when transient-mark-mode
      (transient-mark-mode -1)))
   (t
    (unless transient-mark-mode
      (evil-active-region -1)
      (transient-mark-mode 1)))))

(defun evil-active-region (&optional arg)
  "Toggle active region.
Ensure that Transient Mark mode is properly enabled.
Enable with positive ARG, disable with negative ARG."
  (unless (numberp arg)
    (setq arg (if (region-active-p) -1 1)))
  (cond
   ((and (< arg 1))
    (when (or transient-mark-mode mark-active)
      (setq mark-active nil
            deactivate-mark nil)
      (run-hooks 'deactivate-mark-hook)))
   (t
    (evil-transient-mark 1)
    (when deactivate-mark
      (setq deactivate-mark nil))
    (unless (mark t)
      (evil-move-mark (point)))
    (unless (region-active-p)
      (set-mark (mark t))))))

(defmacro evil-save-transient-mark (&rest body)
  "Save Transient Mark mode; execute BODY; then restore it."
  (declare (indent defun)
           (debug t))
  `(let ((inhibit-quit t)
         evil-transient-vals)
     (unwind-protect
         (progn
           (evil-transient-save)
           ,@body)
       (evil-transient-restore))))

(defmacro evil-save-region (&rest body)
  "Save Transient Mark mode, mark activation, mark and point.
Execute BODY, then restore those things."
  (declare (indent defun)
           (debug t))
  `(evil-save-transient-mark
     (save-excursion
       ,@body)))

(defun evil-normalize-position (pos)
  "Return POS if it does not exceed the buffer boundaries.
If POS is less than `point-min', return `point-min'.
Is POS is more than `point-max', return `point-max'.
If POS is a marker, return its position."
  (cond
   ((not (number-or-marker-p pos))
    pos)
   ((< pos (point-min))
    (point-min))
   ((> pos (point-max))
    (point-max))
   ((markerp pos)
    (marker-position pos))
   (t
    pos)))

;; `set-mark' does too much at once
(defun evil-move-mark (pos)
  "Set buffer's mark to POS.
If POS is nil, delete the mark."
  (when pos
    (setq pos (evil-normalize-position pos)))
  (set-marker (mark-marker) pos))

(evil-define-command evil-exchange-point-and-mark ()
  "Exchange point and mark without activating the region."
  :keep-visual t
  :repeat nil
  (interactive)
  (let* ((point (point))
         (mark  (or (mark t) point)))
    (set-marker (mark-marker) point)
    (goto-char mark)))

(defun evil-adjust-eol (&optional force)
  "Move point one character back if at the end of a non-empty line.
This behavior is contingent on `evil-move-cursor-back';
use the FORCE parameter to override it."
  (when (or evil-move-cursor-back force)
    (when (eolp)
      (evil-adjust))))

(defun evil-adjust ()
  "Move point one character back within the current line."
  (unless (bolp)
    (backward-char)))

(defun evil-apply-on-block (func beg end &rest args)
  "Call FUNC for each line of Visual Block selection.
The selection may be specified explicitly with BEG and END.
FUNC must take at least two arguments, the beginning and end of
each line. Extra arguments to FUNC may be passed via ARGS."
  (let (beg-marker end-marker left right eob)
    (save-excursion
      (evil-sort beg end)
      ;; calculate columns
      (goto-char end)
      (setq right (current-column))
      (goto-char beg)
      (setq left (current-column))
      ;; ensure LEFT < RIGHT
      (when (> left right)
        (evil-sort left right)
        (setq beg (save-excursion
                    (goto-char beg)
                    (move-to-column left)
                    (point))
              end (save-excursion
                    (goto-char end)
                    (move-to-column right)
                    (point))))
      (goto-char beg)
      (setq beg-marker (move-marker (make-marker) beg)
            end-marker (move-marker (make-marker) end))
      (set-marker-insertion-type beg-marker nil)
      (set-marker-insertion-type end-marker t)
      ;; apply FUNC on each line
      (while (progn
               (apply func
                      (save-excursion
                        (move-to-column left t)
                        (point))
                      (save-excursion
                        (move-to-column right t)
                        (point))
                      args)
               (forward-line 1)
               (and (prog1 (not eob)
                      (setq eob (eobp)))
                    (<= (point) end-marker))))
      (set-marker beg-marker nil)
      (set-marker end-marker nil))))

(defun evil-in-comment-p (&optional pos)
  "Whether POS is inside a comment.
POS defaults to the current position of point."
  (setq pos (or pos (point)))
  (and (nth 4 (parse-partial-sexp
               (save-excursion (beginning-of-defun) (point))
               pos)) t))

(defun evil-in-string-p (&optional pos)
  "Whether POS is inside a string.
POS defaults to the current position of point."
  (setq pos (or pos (point)))
  (and (nth 3 (parse-partial-sexp
               (save-excursion (beginning-of-defun) (point))
               pos)) t))

(defun evil-comment-beginning (&optional pos)
  "Return beginning of comment containing POS.
POS defaults to the current position of point."
  (save-excursion
    (goto-char (or pos (point)))
    (when (evil-in-comment-p)
      (while (and (evil-in-comment-p) (not (bobp)))
        (backward-char))
      (point))))

(defun evil-comment-end (&optional pos)
  "Return end of comment containing POS.
POS defaults to the current position of point."
  (save-excursion
    (goto-char (or pos (point)))
    (when (evil-in-comment-p)
      (while (and (evil-in-comment-p) (not (eobp)))
        (forward-char))
      (point))))

(defun evil-string-beginning (&optional pos)
  "Return beginning of string containing POS.
POS defaults to the current position of point."
  (save-excursion
    (goto-char (or pos (point)))
    (when (evil-in-string-p)
      (while (and (evil-in-string-p) (not (bobp)))
        (backward-char))
      (point))))

(defun evil-string-end (&optional pos)
  "Return end of string containing POS.
POS defaults to the current position of point."
  (save-excursion
    (goto-char (or pos (point)))
    (when (evil-in-string-p)
      (while (and (evil-in-string-p) (not (eobp)))
        (forward-char))
      (point))))

(defmacro evil-narrow-to-comment (&rest body)
  "Narrow to the current comment or docstring, if any."
  (declare (indent defun)
           (debug t))
  `(save-restriction
     (cond
      ((evil-in-comment-p)
       (narrow-to-region (evil-comment-beginning) (evil-comment-end)))
      ((evil-in-string-p)
       (narrow-to-region (evil-string-beginning) (evil-string-end))))
     ,@body))

;;; Macro helpers

(eval-and-compile
  (defun evil-unquote (exp)
    "Return EXP unquoted."
    (if (eq (car-safe exp) 'quote)
        (cadr exp)
      exp)))

(defmacro evil-with-or-without-comment (&rest body)
  "Try BODY narrowed to the current comment; then try BODY unnarrowed.
If BODY returns non-nil inside the current comment, return that.
Otherwise, execute BODY again, but without the restriction."
  (declare (indent defun)
           (debug t))
  `(or (when (or (evil-in-comment-p) (evil-in-string-p))
         (evil-narrow-to-comment ,@body))
       (progn ,@body)))

;;; Highlighting

(when (fboundp 'font-lock-add-keywords)
  (font-lock-add-keywords
   'emacs-lisp-mode
   ;; Match all `evil-define-' forms except `evil-define-key'.
   ;; In the interests of speed, this expression is incomplete
   ;; and will not match all three-letter words.
   '(("(\\(evil-define-\\(?:[^ k][^ e][^ y]\\|[-[:word:]]\\{4,\\}\\)\\)\
\\>[ \f\t\n\r\v]*\\(\\sw+\\)?"
      (1 font-lock-keyword-face)
      (2 font-lock-function-name-face nil t))
     ("(\\(evil-\\(?:narrow\\|save\\|with\\)-[-[:word:]]+\\)\\>"
      1 font-lock-keyword-face)
     ("(\\(evil-\\(?:[-[:word:]]\\)*loop\\)\\>"
      1 font-lock-keyword-face))))

(provide 'evil-common)

;;; evil-common.el ends here
