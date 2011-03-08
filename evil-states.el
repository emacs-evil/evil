;;;; State system

;; What is "modes" in Vim is "states" in Evil. States are defined
;; with the macro `evil-define-state'.
;;
;; A state consists of a universal keymap (like
;; `evil-normal-state-map' for Normal state) and a buffer-local keymap for
;; overriding the former (like `evil-normal-state-local-map').
;; Sandwiched between these keymaps may be so-called auxiliary
;; keymaps, which contain state bindings assigned to an Emacs mode
;; (minor or major): more on that below.
;;
;; A state may "inherit" keymaps from another state. For example,
;; Visual state will enable Normal state's keymaps in addition to its own.
;; The keymap order then becomes:
;;
;;     <visual-local-map>
;;     <visual auxiliary maps>
;;     <visual-universal-map>
;;     <normal-local-map>
;;     <normal auxiliary maps>
;;     <normal-universal-map>
;;
;; Since the activation of auxiliary maps depends on the current
;; buffer and its modes, states are necessarily buffer-local.
;; Different buffers can have different states, and different buffers
;; enable states differently. (Thus, what keymaps to enable cannot be
;; determined at compile time.) For example, the user may define some
;; Visual state bindings for foo-mode, and if he enters foo-mode and
;; Visual state in the current buffer, then the auxiliary keymap
;; containing those bindings will be active. In a buffer where
;; foo-mode is not enabled, it will not be.
;;
;; Why go to this trouble? Because it allows state bindings to be
;; grouped into Emacs modes. This is useful for writing extensions.
;;
;; All state keymaps are listed in `evil-mode-map-alist', which is
;; then listed in `emulation-mode-map-alist'. This gives state keymaps
;; precedence over other keymaps. Note that `evil-mode-map-alist'
;; has both a default (global) value and a buffer-local value. The
;; default value is constructed when Evil is loaded and its states
;; are defined. Afterwards, when entering a buffer, the default value
;; is copied into the buffer-local value, and that value is reordered
;; according to the current state (pushing Visual keymaps to the top
;; when the user enters Visual state, etc.).

(require 'evil-vars)
(require 'evil-common)
(require 'evil-repeat)

(defun evil-enable ()
  "Enable Evil in the current buffer, if appropriate.
To enable Evil globally, do (evil-mode 1)."
  ;; TODO: option for enabling vi keys in the minibuffer
  (unless (minibufferp)
    (evil-local-mode 1)))

(define-minor-mode evil-local-mode
  "Minor mode for setting up Evil in a single buffer."
  :init-value nil
  (cond
   (evil-local-mode
    (setq emulation-mode-map-alists
          (evil-concat-lists '(evil-mode-map-alist)
                             emulation-mode-map-alists))
    (evil-refresh-local-maps)
    (unless (memq 'evil-modeline-tag global-mode-string)
      (setq global-mode-string
            (append '("" evil-modeline-tag)
                    global-mode-string)))
    (evil-normal-state))
   (t
    (when evil-state
      (funcall (evil-state-func) -1)))))

(define-globalized-minor-mode evil-mode
  evil-local-mode evil-enable)

(put 'evil-mode 'function-documentation
     "Toggle Evil in all buffers.
Enable with positive ARG and disable with negative ARG.
See `evil-local-mode' to toggle Evil in the
current buffer only.")

(defun evil-state-property (state prop)
  "Return property PROP for STATE."
  (evil-get-property evil-states-alist state prop))

(defun evil-state-p (sym)
  "Whether SYM is the name of a state."
  (assq sym evil-states-alist))

(defun evil-state-func (&optional state)
  "Return the toggle function for STATE."
  (setq state (or state evil-state))
  (evil-state-property state :mode))

(defun evil-state-keymaps (state &rest excluded)
  "Return an ordered list of keymaps activated by STATE."
  (let* ((state (or state evil-state))
         (map (symbol-value (evil-state-property state :keymap)))
         (local-map (symbol-value (evil-state-property
                                   state :local-keymap)))
         (aux-maps (evil-state-auxiliary-keymaps state))
         (enable (evil-state-property state :enable))
         (excluded (add-to-list 'excluded state))
         ;; the keymaps for STATE
         (result (append (list local-map) aux-maps (list map))))
    ;; the keymaps for other states and modes enabled by STATE
    (dolist (entry enable result)
      (cond
       ((evil-state-p entry)
        (unless (memq entry excluded)
          (dolist (mode (apply 'evil-state-keymaps entry excluded))
            (add-to-list 'result mode t 'eq))))
       ((keymapp entry)
        (add-to-list 'result entry t 'eq))
       ((keymapp (symbol-value entry))
        (add-to-list 'result (symbol-value entry) t 'eq))
       (t
        (setq map (cdr (or (assq entry evil-mode-map-alist)
                           (assq entry minor-mode-map-alist))))
        (when map
          (add-to-list 'result map t 'eq)))))))

(defun evil-state-auxiliary-keymaps (state)
  "Return an ordered list of auxiliary keymaps for STATE."
  (let* ((state (or state evil-state))
         (alist (symbol-value (evil-state-property state :aux)))
         result)
    (dolist (map (current-active-maps) result)
      (when (keymapp (setq map (cdr (assq map alist))))
        (add-to-list 'result map t 'eq)))))

(defun evil-normalize-keymaps (&optional state)
  "Create a buffer-local value for `evil-mode-map-alist'.
Its order reflects the state in the current buffer."
  (let ((state (or state evil-state))
        (modes (evil-concat-lists
                (mapcar 'cdr (evil-state-property nil :mode))
                (mapcar 'cdr (evil-state-property nil :local-mode))))
        alist mode)
    ;; initialize a buffer-local value
    (setq evil-mode-map-alist
          (copy-sequence (default-value 'evil-mode-map-alist)))
    ;; update references to buffer-local keymaps
    (evil-refresh-local-maps)
    ;; disable all modes
    (dolist (entry (evil-concat-lists evil-mode-map-alist
                                      evil-local-keymaps-alist))
      (setq mode (car entry))
      (if (and (fboundp mode)
               (not (memq mode modes)))
          (funcall mode -1)
        (set mode nil)))
    ;; enable modes for current state
    (unless (null state)
      (dolist (map (evil-state-keymaps state))
        (when (setq mode (or (car (rassq map evil-mode-map-alist))
                             (car (rassq map minor-mode-map-alist))))
          (if (and (fboundp mode)
                   (not (memq mode modes)))
              (funcall mode 1)
            (set mode t))
          ;; refresh the keymap for good measure (buffer-local keymaps
          ;; may change in the toggle function itself)
          (setq map (or (cdr (assq mode evil-mode-map-alist))
                        (cdr (assq mode minor-mode-map-alist))
                        map))
          (add-to-list 'alist (cons mode map) t)))
      ;; move the enabled modes to the front of the list
      (setq evil-mode-map-alist
            (evil-concat-lists
             alist evil-mode-map-alist)))))

;; Local keymaps are implemented using buffer-local variables.
;; However, unless a buffer-local value already exists,
;; `define-key' acts on the variable's default (global) value.
;; So we need to initialize the variable whenever we enter a
;; new buffer or when the buffer-local values are reset.
(defun evil-refresh-local-maps ()
  "Initialize a buffer-local value for all local keymaps."
  (unless (assq 'evil-mode-map-alist (buffer-local-variables))
    (setq evil-mode-map-alist (copy-sequence evil-mode-map-alist)))
  (dolist (entry evil-local-keymaps-alist)
    (let ((mode (car entry))
          (map  (cdr entry)))
      (unless (and (keymapp (symbol-value map))
                   (assq map (buffer-local-variables)))
        (set map (make-sparse-keymap)))
      (evil-add-to-alist 'evil-mode-map-alist
                         mode (symbol-value map)))))

(defun evil-set-cursor (specs)
  "Change the cursor's apperance according to SPECS.
SPECS may be a cursor type as per `cursor-type', a color
string as passed to `set-cursor-color', a zero-argument
function for changing the cursor, or a list of the above.
If SPECS is nil, make the cursor a black filled box."
  (set-cursor-color "black")
  (setq cursor-type 'box)
  (unless (and (listp specs) (not (consp specs)))
    (setq specs (list specs)))
  (dolist (spec specs)
    (cond
     ((functionp spec)
      (condition-case nil
          (funcall spec)
        (error nil)))
     ((stringp spec)
      (set-cursor-color spec))
     (t
      (setq cursor-type spec))))
  (redisplay))

(defun evil-change-state (state)
  "Change state to STATE.
Disable all states if nil."
  (let ((func (evil-state-property
               (or state 'emacs) :mode)))
    (unless (eq state evil-state)
      (funcall func (if state 1 -1)))))

(defmacro evil-define-keymap (keymap doc &rest body)
  "Define a keymap KEYMAP listed in `evil-mode-map-alist'.
That means it will have precedence over regular keymaps.

DOC is the documentation for the variable. BODY, if specified,
is executed after toggling the mode. Optional keyword arguments
may be specified before the body code:

:mode VAR       Mode variable. If unspecified, the variable
                is based on the keymap name.
:local BOOLEAN  Whether the keymap should be buffer-local, that is,
                reinitialized for each buffer.
:func BOOLEAN   Create a toggle function even if BODY is empty."
  (declare (debug (&define name
                           [&optional stringp]
                           [&rest [keywordp sexp]]
                           def-body))
           (indent defun))
  (let (func mode keyword local)
    (while (keywordp (car-safe body))
      (setq keyword (pop body))
      (cond
       ((eq keyword :mode)
        (setq mode (pop body)))
       ((eq keyword :local)
        (setq local (pop body)))
       ((eq keyword :func)
        (setq func (pop body)))
       (t
        (pop body)))
      (setq mode (or mode
                     (intern (replace-regexp-in-string
                              "\\(?:-\\(?:key\\)?map\\)?$" "-mode"
                              (symbol-name keymap))))))
    `(progn
       (defvar ,keymap nil)
       (unless (get ',keymap 'variable-documentation)
         (put ',keymap 'variable-documentation ,doc))
       (defvar ,mode nil)
       (unless (get ',mode 'variable-documentation)
         (put ',mode 'variable-documentation ,doc))
       (make-variable-buffer-local ',mode)
       ,@(when local
           `((make-variable-buffer-local ',keymap)
             (evil-add-to-alist 'evil-local-keymaps-alist
                                ',mode ',keymap)))
       (let ((temp (copy-sequence (default-value
                                    'evil-mode-map-alist))))
         (evil-add-to-alist 'temp ',mode ,keymap)
         (setq-default evil-mode-map-alist temp))
       ,(when (or body func)
          `(defun ,mode (&optional arg)
             ,@(when doc `(,doc))
             (interactive)
             (cond
              ((numberp arg)
               (setq ,mode (> arg 0)))
              (t
               (setq ,mode (not ,mode))))
             ,@body))
       ',keymap)))

(defmacro evil-define-state (state doc &rest body)
  "Define a Evil state STATE.
DOC is a general description and shows up in all docstrings.
Then follows one or more optional keywords:

:tag STRING             Mode line indicator.
:message STRING         Echo area message when changing to STATE.
:cursor SPEC            Cursor to use in STATE.
:entry-hook LIST        Hooks run when changing to STATE.
:exit-hook LIST         Hooks run when changing from STATE.
:enable LIST            List of other states and modes enabled by STATE.
:suppress-keymap FLAG   If FLAG is non-nil, makes
                        evil-suppress-map the parent of the
                        global map of STATE effectively disabling
                        bindings to self-insert-command.

Following the keywords is optional code to be executed each time
the state is enabled or disabled.

For example:

    (evil-define-state test
      \"A simple test state.\"
      :tag \"<T> \")

The basic keymap of this state will then be
`evil-test-state-map', and so on."
  (declare (debug (&define name
                           [&optional stringp]
                           [&rest [keywordp sexp]]
                           def-body))
           (indent defun))
  (let ((mode (intern (format "evil-%s-state" state)))
        (keymap (intern (format "evil-%s-state-map" state)))
        (local-mode (intern (format "evil-%s-state-local" state)))
        (local-keymap (intern (format "evil-%s-state-local-map" state)))
        (aux (intern (format "evil-%s-state-auxiliary-maps" state)))
        (predicate (intern (format "evil-%s-state-p" state)))
        (tag (intern (format "evil-%s-state-tag" state)))
        (message (intern (format "evil-%s-state-message" state)))
        (cursor (intern (format "evil-%s-state-cursor" state)))
        (entry-hook (intern (format "evil-%s-state-entry-hook" state)))
        (exit-hook (intern (format "evil-%s-state-exit-hook" state)))
        cursor-value enable entry-hook-value exit-hook-value keyword
        message-value tag-value suppress-keymap)
    ;; collect keywords
    (while (keywordp (car-safe body))
      (setq keyword (pop body))
      (cond
       ((eq keyword :tag)
        (setq tag-value (pop body)))
       ((eq keyword :message)
        (setq message-value (pop body)))
       ((eq keyword :cursor)
        (setq cursor-value (pop body)))
       ((eq keyword :entry-hook)
        (setq entry-hook-value (pop body)))
       ((eq keyword :exit-hook)
        (setq exit-hook-value (pop body)))
       ((eq keyword :enable)
        (setq enable (pop body)))
       ((eq keyword :suppress-keymap)
        (setq suppress-keymap (pop body)))
       (t
        (pop body))))

    ;; macro expansion
    `(progn
       ;; Save the state's properties in `evil-states-alist' for
       ;; runtime lookup. Among other things, this information is used
       ;; to determine what keymaps should be activated by the state
       ;; (and, when processing :enable, what keymaps are activated by
       ;; other states). We cannot know this at compile time because
       ;; it depends on the current buffer and its active keymaps
       ;; (to which we may have assigned state bindings), as well as
       ;; states whose definitions may not have been processed yet.
       (evil-put-property
        'evil-states-alist ',state
        :tag (defvar ,tag ,tag-value
               ,(format "Modeline tag for %s state.\n\n%s" state doc))
        :message (defvar ,message ,message-value
                   ,(format "Echo area indicator for %s state.\n\n%s"
                            state doc))
        :cursor (defvar ,cursor ',cursor-value
                  ,(format "Cursor for %s state.
May be a cursor type as per `cursor-type', a color string as passed
to `set-cursor-color', a zero-argument function for changing the
cursor, or a list of the above.\n\n%s" state doc))
        :entry-hook (defvar ,entry-hook ,entry-hook-value
                      ,(format "Hooks to run when entering %s state.\n\n%s"
                               state doc))
        :exit-hook (defvar ,exit-hook ,exit-hook-value
                     ,(format "Hooks to run when exiting %s state.\n\n%s"
                              state doc))
        :mode (defvar ,mode nil
                ,(format "Non-nil if %s state is enabled.
Use the command `%s' to change this variable.\n\n%s" state mode doc))
        :keymap (defvar ,keymap (make-sparse-keymap)
                  ,(format "Keymap for %s state.\n\n%s" state doc))
        :local-mode (defvar ,local-mode nil
                      ,(format "Non-nil if %s state is enabled.
Use the command `%s' to change this variable.\n\n%s" state mode doc))
        :local-keymap (defvar ,local-keymap nil
                        ,(format "Buffer-local keymap for %s state.\n\n%s"
                                 state doc))
        :aux (defvar ,aux nil
               ,(format "Association list of auxiliary keymaps for %s state.
Elements have the form (KEYMAP . AUX-MAP), where AUX-MAP contains state
bindings to be activated whenever KEYMAP and %s state are active."
                        state state))
        :predicate (defun ,predicate ()
                     ,(format "Whether the current state is %s." state)
                     (eq evil-state ',state))
        :enable ',enable)

       ,@(when suppress-keymap
           `((set-keymap-parent ,keymap evil-suppress-map)))

       ;; define state function
       (defun ,mode (&optional arg)
         ,(format "Enable %s state. Disable with negative ARG.\n\n%s"
                  state doc)
         (interactive)
         (cond
          ((and (numberp arg) (< arg 1))
           (unwind-protect
               (let ((evil-state evil-state))
                 (run-hooks ',exit-hook)
                 (setq evil-state nil)
                 (evil-normalize-keymaps)
                 ,@body)
             (setq evil-state nil)))
          (t
           (unless evil-local-mode
             (evil-enable))
           (when evil-state
             (funcall (evil-state-func) -1))
           (unwind-protect
               (let ((evil-state ',state))
                 (evil-normalize-keymaps)
                 (setq evil-modeline-tag ,tag)
                 (force-mode-line-update)
                 (evil-set-cursor ,cursor)
                 ,@body
                 (run-hooks ',entry-hook)
                 (when (called-interactively-p)
                   (when ,message (evil-unlogged-message ,message))))
             (setq evil-state ',state)))))

       (evil-define-keymap ,local-keymap nil
         :mode ,local-mode
         :local t)

       (evil-define-keymap ,keymap nil
         :mode ,mode)

       ',state)))

;; Define states

(evil-define-state normal
  "Normal state, AKA \"Command\" state."
  :tag " <N> "
  :suppress-keymap t
  :enable (operator)
  (if evil-state
      (evil-setup-normal-repeat)
    (evil-teardown-normal-repeat)))

(evil-define-state visual
  "Visual state."
  :tag " <V> ")

(evil-define-state emacs
  "Emacs state."
  :tag " <E> ")

;; TODO: the following commands are very preliminary just for testing.
(defun evil-replace-char (char &optional count)
  (interactive (list (read-char)
                     (prefix-numeric-value current-prefix-arg)))
  (setq count (or count 1))
  (delete-char count)
  (insert-char char count)
  (backward-char))

(provide 'evil-states)

;;; evil-states.el ends here
