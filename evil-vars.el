;;; Settings and variables

(defvar evil-state nil
  "The current Evil state.
To change the state, use `evil-change-state'
or call the state function (e.g., `evil-normal-state').")
(make-variable-buffer-local 'evil-state)

(defvar evil-modeline-tag nil
  "Modeline indicator for the current state.")
(make-variable-buffer-local 'evil-modeline-tag)

(defvar evil-local-keymaps-alist nil
  "Association list of keymap variables that must be
reinitialized in each buffer. Entries have the form
\(MODE . KEYMAP), where KEYMAP is the variable containing
the keymap for MODE.")

(defvar evil-states-alist nil
  "Specifications made by `evil-define-state'.
Entries have the form (STATE . PLIST), where PLIST is a property
list specifying various aspects of the state. To access a property,
use `evil-state-property'.")

(defvar evil-mode-map-alist nil
  "Association list of keymaps to use for Evil modes.
Elements have the form (MODE . KEYMAP), with the first keymaps
having higher priority.")
(make-variable-buffer-local 'evil-mode-map-alist)

(defvar evil-transient-vars '(transient-mark-mode mark-active)
  "List of variables pertaining to Transient Mark mode.")

(defvar evil-transient-vals nil
  "Association list of old values for Transient Mark mode variables.
Entries have the form (VARIABLE VALUE LOCAL), where LOCAL is
whether the variable was previously buffer-local.")

(defvar evil-types-alist nil
  "Specifications made by `evil-define-type'.
Entries have the form (TYPE . PLIST), where PLIST is a property
list specifying functions for handling the type: expanding it,
describing it, etc.")

(defvar evil-motion-marker nil
  "Marker for storing the starting position of a motion.")
(make-variable-buffer-local 'evil-mode-map-alist)

(defvar evil-this-type nil
  "Current motion type.")
(make-variable-buffer-local 'evil-mode-map-alist)

(defvar evil-this-motion nil
  "Current motion.")
(make-variable-buffer-local 'evil-mode-map-alist)

(defvar evil-this-motion-count nil
  "Current motion count.")
(make-variable-buffer-local 'evil-mode-map-alist)

(defvar evil-motions nil
  "List of motion commands.")

(defvar evil-operators nil
  "List of operator commands.")

(defvar evil-inhibit-operator nil
  "Inhibit current operator.
If an operator calls a motion and the motion sets this variable
to t, the operator code is not executed.")

(defconst evil-suppress-map (make-keymap)
  "Full keymap disabling default bindings to self-insert-command.")
(suppress-keymap evil-suppress-map t)

(defvar evil-repeat-info nil
  "A list if repeat-informations to repeat the last command.")

(defvar evil-repeating-command nil
  "This variable is non-nil if a command is currently being repeated.")

(defvar evil-insert-repeat-info nil
  "Repeat information accumulated during insert mode.")

(defvar evil-insert-repeat-type nil
  "The repeat-type of the current command. If set to 'change the
command will be recorded by tracking the changes, if set to nil
by tracking the key-sequences, if set to 'ignore the command is
ignored.")
(make-variable-buffer-local 'evil-insert-repeat-type)

(defvar evil-insert-repeat-point nil
  "The position of point at the beginning of an change-tracking
  editing command.")
(make-variable-buffer-local 'evil-insert-repeat-point)

(defvar evil-insert-repeat-types (make-hash-table :test 'eq)
  "The hash-table to hold the insert repeat type for each
  command.")

(defvar evil-command-modified-buffer nil
  "Non-nil if the current command modified the buffer, i.e., it
  is an editing command. This variable is used to detect editing
  command for repeation.")

(defvar evil-repeat-count nil
  "The explicit count when repeating a command.")

(defvar evil-insert-count nil
  "The explicit count passed to an command starting insert mode.")
(make-variable-buffer-local 'evil-insert-count)

(defvar evil-insert-lines nil
  "Non-nil if the current insertion command is a line-insertion
command o or O.")
(make-variable-buffer-local 'evil-insert-lines)

(defvar evil-write-echo-area nil
  "If set to t inside `evil-save-echo-area', then the echo area
is not restored.")

(defvar evil-word "a-zA-Z0-9_"
  "The characters to be considered as a word.")

(defconst evil-version "0.1"
  "The current version of Evil")

(defun evil-version ()
  (interactive)
  (message "Evil version %s" evil-version))

(provide 'evil-vars)

;;; evil-vars.el ends here
