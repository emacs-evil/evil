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

(require 'evil-common)

(define-minor-mode evil-local-mode
  "Minor mode for setting up Evil in a single buffer."
  :init-value nil
  (cond
   (evil-local-mode
    (setq emulation-mode-map-alists
          (evil-concat-lists '(evil-mode-map-alist)
                             emulation-mode-map-alists))
    (evil-refresh-local-keymaps)
    (unless (memq 'evil-modeline-tag global-mode-string)
      (setq global-mode-string
            (append '("" evil-modeline-tag)
                    global-mode-string)))
    (ad-enable-advice 'show-paren-function 'around 'evil)
    (ad-activate 'show-paren-function)
    ;; restore the proper value of `major-mode' in Fundamental buffers
    (when (eq major-mode 'evil-local-mode)
      (setq major-mode 'fundamental-mode))
    ;; determine and enable the initial state
    (evil-initialize-state)
    ;; re-determine the initial state in `post-command-hook' since the
    ;; major mode may not be initialized yet, and some modes neglect
    ;; to run `after-change-major-mode-hook'
    (add-hook 'post-command-hook 'evil-initialize-state t t)
    (add-hook 'after-change-functions 'evil-repeat-change-hook nil t)
    (add-hook 'pre-command-hook 'evil-repeat-pre-hook nil t)
    (add-hook 'post-command-hook 'evil-repeat-post-hook nil t)
    (add-hook 'post-command-hook 'evil-refresh-cursor))
   (t
    (let (new-global-mode-string)
      (while global-mode-string
        (let ((next (pop global-mode-string)))
          (if (eq next 'evil-modeline-tag)
              (pop new-global-mode-string) ;; remove the ""
            (push next new-global-mode-string))))
      (setq global-mode-string (nreverse new-global-mode-string)))
    (ad-disable-advice 'show-paren-function 'around 'evil)
    (ad-activate 'show-paren-function)
    (evil-change-state nil))))

(defun evil-initialize ()
  "Enable Evil in the current buffer, if appropriate.
To enable Evil globally, do (evil-mode 1)."
  ;; TODO: option for enabling vi keys in the minibuffer
  (unless (minibufferp)
    (evil-local-mode 1)
    (remove-hook 'post-command-hook 'evil-initialize-state t)))

(define-globalized-minor-mode evil-mode
  evil-local-mode evil-initialize)

;; to ensure that Fundamental buffers come up in Normal state,
;; initialize `fundamental-mode' via `evil-local-mode'
(defadvice evil-mode (after evil activate)
  "Enable Evil in Fundamental mode."
  (if evil-mode
      ;; this is changed back when initializing `evil-local-mode'
      (setq-default major-mode 'evil-local-mode)
    (setq-default major-mode 'fundamental-mode)))

(put 'evil-mode 'function-documentation
     "Toggle Evil in all buffers.
Enable with positive ARG and disable with negative ARG.
See `evil-local-mode' to toggle Evil in the
current buffer only.")

(defun evil-state-p (sym)
  "Whether SYM is the name of a state."
  (assq sym evil-state-properties))

(defun evil-initialize-state (&optional buffer)
  "Initialize Evil state in BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (evil-change-to-initial-state buffer)
    (remove-hook 'post-command-hook 'evil-initialize-state t)))

(defun evil-change-to-initial-state (&optional buffer message)
  "Change state to the initial state for BUFFER.
This is the state the buffer comes up in."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (evil-change-state (evil-initial-state-for-buffer buffer 'normal)
                       message)))

(defun evil-change-to-previous-state (&optional buffer message)
  "Change the state of BUFFER to its previous state."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (evil-change-state (or evil-previous-state evil-state 'normal)
                       message)))

(defun evil-exit-emacs-state (&optional buffer message)
  "Change from Emacs state to the previous state."
  (interactive '(nil t))
  (with-current-buffer (or buffer (current-buffer))
    (evil-change-to-previous-state buffer message)
    (when (evil-emacs-state-p)
      (evil-normal-state (and message 1)))))

(defun evil-change-state (state &optional message)
  "Change state to STATE.
Disable all states if nil."
  (let ((func (evil-state-property (or state evil-state) :toggle)))
    (when (and (functionp func)
               (not (eq state evil-state)))
      (funcall func (if state (and message 1) -1)))))

(defun evil-state-keymaps (state &rest excluded)
  "Return an ordered list of keymaps activated by STATE.
Skip states listed in EXCLUDED."
  (let* ((state (or state evil-state))
         (map (symbol-value (evil-state-property state :keymap)))
         (local-map (symbol-value (evil-state-property
                                   state :local-keymap)))
         (aux-maps (evil-state-auxiliary-keymaps state))
         (enable (evil-state-property state :enable))
         result)
    (unless (memq state enable)
      (add-to-list 'enable state))
    ;; the keymaps for other states and modes enabled by STATE
    (dolist (entry enable result)
      (cond
       ((memq entry excluded))
       ((eq entry state)
        (setq result
              (evil-concat-lists
               result
               (list local-map) aux-maps (list map)))
        (add-to-list 'excluded state))
       ((evil-state-p entry)
        (setq result (evil-concat-lists
                      result
                      (apply 'evil-state-keymaps entry excluded))))
       ((keymapp entry)
        (add-to-list 'result entry t 'eq))
       ((keymapp (symbol-value entry))
        (add-to-list 'result (symbol-value entry) t 'eq))
       (t
        (setq map (evil-mode-keymap entry))
        (when map
          (add-to-list 'result map t 'eq)))))))

(defun evil-normalize-keymaps (&optional state)
  "Create a buffer-local value for `evil-mode-map-alist'.
Its order reflects the state in the current buffer."
  (let ((state (or state evil-state))
        alist mode)
    (evil-refresh-global-keymaps)
    (evil-refresh-local-keymaps)
    ;; disable all modes
    (dolist (mode (mapcar 'car (append evil-mode-map-alist
                                       evil-local-keymaps-alist)))
      (when (fboundp mode)
        (funcall mode -1))
      (set mode nil))
    ;; enable modes for current state
    (when state
      (dolist (map (evil-state-keymaps state))
        (if (evil-auxiliary-keymap-p map)
            (add-to-list 'alist (cons t map) t)
          (when (setq mode (evil-keymap-mode map))
            (when (fboundp mode)
              (funcall mode 1))
            (set mode t)
            ;; refresh the keymap in case it has changed
            ;; (e.g., `evil-operator-shortcut-map' is
            ;; reset on toggling)
            (setq map (or (evil-mode-keymap mode) map))
            (evil-add-to-alist 'alist mode map)))))
    ;; move the enabled modes to the front of the list
    (setq evil-mode-map-alist
          (evil-filter-list (lambda (elt)
                              (assq (car-safe elt) alist))
                            evil-mode-map-alist))
    (setq evil-mode-map-alist (append alist evil-mode-map-alist))))

(defun evil-refresh-global-keymaps ()
  "Refresh the global value of `evil-mode-map-alist'.
Update its entries if keymaps change."
  (let ((temp (default-value 'evil-mode-map-alist))
        mode map)
    (dolist (entry evil-global-keymaps-alist)
      (setq mode (car entry)
            map  (cdr entry))
      (evil-add-to-alist 'temp mode (symbol-value map)))
    (setq-default evil-mode-map-alist temp)))

;; Local keymaps are implemented using buffer-local variables.
;; However, unless a buffer-local value already exists,
;; `define-key' acts on the variable's default (global) value.
;; So we need to initialize the variable whenever we enter a
;; new buffer or when the buffer-local values are reset.
(defun evil-refresh-local-keymaps ()
  "Refresh the buffer-local value of `evil-mode-map-alist'.
Initialize a buffer-local value for all local keymaps
and update their list entries."
  (setq evil-mode-map-alist
        (copy-sequence (default-value 'evil-mode-map-alist)))
  (dolist (entry evil-local-keymaps-alist)
    (let ((mode (car entry))
          (map  (cdr entry)))
      (unless (and (keymapp (symbol-value map))
                   (assq map (buffer-local-variables)))
        (set map (make-sparse-keymap)))
      (evil-add-to-alist 'evil-mode-map-alist
                         mode (symbol-value map)))))

(defun evil-keymap-mode (keymap)
  "Return minor mode for KEYMAP.
See also `evil-mode-keymap'."
  (let ((map (if (keymapp keymap) keymap (symbol-value keymap)))
        (var (when (symbolp keymap) keymap)))
    (or (when var
          (or (car (rassq var evil-global-keymaps-alist))
              (car (rassq var evil-local-keymaps-alist))))
        (car (rassq map (mapcar (lambda (e)
                                  ;; from (MODE-VAR . MAP-VAR)
                                  ;; to (MODE-VAR . MAP)
                                  (cons (car-safe e)
                                        (symbol-value (cdr-safe e))))
                                (append evil-global-keymaps-alist
                                        evil-local-keymaps-alist))))
        (car (rassq map minor-mode-map-alist)))))

(defun evil-mode-keymap (mode &optional variable)
  "Return keymap for minor MODE.
Return the keymap variable if VARIABLE is non-nil.
See also `evil-keymap-mode'."
  (let* ((var (or (cdr (assq mode evil-global-keymaps-alist))
                  (cdr (assq mode evil-local-keymaps-alist))))
         (map (or (symbol-value var)
                  (cdr (assq mode minor-mode-map-alist)))))
    (if variable var map)))

(defun evil-state-auxiliary-keymaps (state)
  "Return an ordered list of auxiliary keymaps for STATE."
  (let* ((state (or state evil-state))
         aux result)
    (dolist (map (current-active-maps) result)
      (when (setq aux (evil-get-auxiliary-keymap map state))
        (add-to-list 'result aux t 'eq)))))

(defun evil-set-auxiliary-keymap (map state &optional aux)
  "Set the auxiliary keymap for MAP in STATE to AUX.
If AUX is nil, create a new auxiliary keymap."
  (unless (keymapp aux)
    (setq aux (make-sparse-keymap)))
  (unless (evil-auxiliary-keymap-p aux)
    (evil-set-keymap-prompt
     aux (format "Auxiliary keymap for %s state" state)))
  (define-key map
    (vconcat (list (intern (format "%s-state" state)))) aux)
  aux)

(defun evil-get-auxiliary-keymap (map state &optional create)
  "Get the auxiliary keymap for MAP in STATE.
If CREATE is non-nil, create an auxiliary keymap
if MAP does not have one."
  (when state
    (let* ((key (vconcat (list (intern (format "%s-state" state)))))
           (aux (lookup-key map key)))
      (cond
       ((evil-auxiliary-keymap-p aux)
        aux)
       (create
        (evil-set-auxiliary-keymap map state))))))

(defun evil-auxiliary-keymap-p (map)
  "Whether MAP is an auxiliary keymap."
  (and (keymapp map)
       (string-match "Auxiliary keymap"
                     (or (keymap-prompt map) "")) t))

(defun evil-define-key (state keymap key def)
  "Create a STATE binding from KEY to DEF for KEYMAP.
The syntax is similar to that of `define-key'. For example:

    (evil-define-key 'normal foo-map \"a\" 'bar)

This creates a binding from \"a\" to `bar' in Normal state,
which is active whenever `foo-map' is active."
  (let ((aux (if state
                 (evil-get-auxiliary-keymap keymap state t)
               keymap)))
    (define-key aux key def)
    ;; ensure the prompt string comes first
    (evil-set-keymap-prompt aux (keymap-prompt aux))))

(put 'evil-define-key 'lisp-indent-function 'defun)
(put 'evil-set-auxiliary-keymap 'lisp-indent-function 'defun)

;; these may be useful for programmatic purposes
(defun evil-global-set-key (state key def)
  "Bind KEY to DEF in STATE."
  (define-key (symbol-value (evil-state-property state :keymap))
    key def))

(defun evil-local-set-key (state key def)
  "Bind KEY to DEF in STATE in the current buffer."
  (define-key (symbol-value (evil-state-property state :local-keymap))
    key def))

(defun evil-initial-state-for-buffer (&optional buffer default)
  "Return initial Evil state to use for BUFFER, or DEFAULT if none.
BUFFER defaults to the current buffer."
  (let (state)
    (with-current-buffer (or buffer (current-buffer))
      (or (catch 'loop
            (dolist (mode (append (mapcar 'car minor-mode-map-alist)
                                  (list major-mode)))
              (when (and (or (not (boundp mode)) (symbol-value mode))
                         (setq state (evil-initial-state mode)))
                (throw 'loop state))))
          default))))

(defun evil-initial-state (mode &optional default)
  "Return Evil state to use for MODE, or DEFAULT if none.
The initial state for a mode can be set with
`evil-set-initial-state'."
  (let (state modes)
    (or (catch 'loop
          (dolist (entry (evil-state-property nil :modes))
            (setq state (car entry)
                  modes (symbol-value (cdr entry)))
            (when (memq mode modes)
              (throw 'loop state))))
        default)))

(defun evil-set-initial-state (mode state)
  "Set the initial state for MODE to STATE.
This is the state the buffer comes up in."
  (dolist (modes (evil-state-property nil :modes))
    (setq modes (cdr-safe modes))
    (set modes (delq mode (symbol-value modes))))
  (when state
    (add-to-list (evil-state-property state :modes) mode)))

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
:func BOOLEAN   Create a toggle function even if BODY is empty.

\(fn KEYMAP DOC [[KEY VAL]...] BODY...)"
  (declare (indent defun)
           (debug (&define name
                           [&optional stringp]
                           [&rest [keywordp sexp]]
                           def-body)))
  (let (arg func key local mode)
    (while (keywordp (car-safe body))
      (setq key (pop body)
            arg (pop body))
      (cond
       ((eq key :mode)
        (setq mode arg))
       ((eq key :local)
        (setq local arg))
       ((eq key :func)
        (setq func arg))))
    (setq mode (or mode
                   (intern (replace-regexp-in-string
                            "\\(?:-\\(?:mode-\\)?\\(?:key\\)?map\\)?$"
                            "-mode"
                            (symbol-name keymap)))))
    `(progn
       (defvar ,keymap ,(unless local '(make-sparse-keymap)))
       (unless (get ',keymap 'variable-documentation)
         (put ',keymap 'variable-documentation ,doc))
       (defvar ,mode nil)
       (unless (get ',mode 'variable-documentation)
         (put ',mode 'variable-documentation ,doc))
       (make-variable-buffer-local ',mode)
       ,@(if local
             `((make-variable-buffer-local ',keymap)
               (evil-add-to-alist 'evil-local-keymaps-alist
                                  ',mode ',keymap))
           `((evil-add-to-alist 'evil-global-keymaps-alist
                                ',mode ',keymap)
             (evil-add-to-alist 'evil-mode-map-alist
                                ',mode ,keymap)))
       (evil-refresh-global-keymaps)
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
  "Define an Evil state STATE.
DOC is a general description and shows up in all docstrings.
Then follows one or more optional keywords:

:tag STRING             Mode line indicator.
:message STRING         Echo area message when changing to STATE.
:cursor SPEC            Cursor to use in STATE.
:entry-hook LIST        Hooks run when changing to STATE.
:exit-hook LIST         Hooks run when changing from STATE.
:enable LIST            List of other states and modes enabled by STATE.
:suppress-keymap FLAG   If FLAG is non-nil, makes
                        `evil-suppress-map' the parent of the
                        global map of STATE, effectively disabling
                        bindings to `self-insert-command'.

Following the keywords is optional code to be executed each time
the state is enabled or disabled.

For example:

    (evil-define-state test
      \"A simple test state.\"
      :tag \"<T> \")

The basic keymap of this state will then be
`evil-test-state-map', and so on.

\(fn STATE DOC [[KEY VAL]...] BODY...)"
  (declare (indent defun)
           (debug (&define name
                           [&optional stringp]
                           [&rest [keywordp sexp]]
                           def-body)))
  (let* ((toggle (intern (format "evil-%s-state" state)))
         (mode (intern (format "%s-minor-mode" toggle)))
         (keymap (intern (format "%s-map" toggle)))
         (local (intern (format "%s-local-minor-mode" toggle)))
         (local-keymap (intern (format "%s-local-map" toggle)))
         (tag (intern (format "%s-tag" toggle)))
         (message (intern (format "%s-message" toggle)))
         (cursor (intern (format "%s-cursor" toggle)))
         (entry-hook (intern (format "%s-entry-hook" toggle)))
         (exit-hook (intern (format "%s-exit-hook" toggle)))
         (modes (intern (format "%s-modes" toggle)))
         (predicate (intern (format "%s-p" toggle)))
         arg cursor-value enable entry-hook-value exit-hook-value
         key message-value suppress-keymap tag-value)
    ;; collect keywords
    (while (keywordp (car-safe body))
      (setq key (pop body)
            arg (pop body))
      (cond
       ((eq key :tag)
        (setq tag-value arg))
       ((eq key :message)
        (setq message-value arg))
       ((eq key :cursor)
        (setq cursor-value arg))
       ((eq key :entry-hook)
        (setq entry-hook-value arg)
        (unless (listp entry-hook-value)
          (setq entry-hook-value (list entry-hook-value))))
       ((eq key :exit-hook)
        (setq exit-hook-value arg)
        (unless (listp exit-hook-value)
          (setq exit-hook-value (list entry-hook-value))))
       ((eq key :enable)
        (setq enable arg))
       ((eq key :suppress-keymap)
        (setq suppress-keymap arg))))

    ;; macro expansion
    `(progn
       ;; Save the state's properties in `evil-state-properties' for
       ;; runtime lookup. Among other things, this information is used
       ;; to determine what keymaps should be activated by the state
       ;; (and, when processing :enable, what keymaps are activated by
       ;; other states). We cannot know this at compile time because
       ;; it depends on the current buffer and its active keymaps
       ;; (to which we may have assigned state bindings), as well as
       ;; states whose definitions may not have been processed yet.
       (evil-put-property
        'evil-state-properties ',state
        :toggle ',toggle
        :mode (defvar ,mode nil
                ,(format "Non-nil if %s state is enabled.
Use the command `%s' to change this variable.\n\n%s" state toggle doc))
        :keymap (defvar ,keymap (make-sparse-keymap)
                  ,(format "Keymap for %s state.\n\n%s" state doc))
        :local (defvar ,local nil
                 ,(format "Non-nil if %s state is enabled.
Use the command `%s' to change this variable.\n\n%s" state toggle doc))
        :local-keymap (defvar ,local-keymap nil
                        ,(format "Buffer-local keymap for %s state.\n\n%s"
                                 state doc))
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
        :entry-hook (defvar ,entry-hook nil
                      ,(format "Hooks to run when entering %s state.\n\n%s"
                               state doc))
        :exit-hook (defvar ,exit-hook nil
                     ,(format "Hooks to run when exiting %s state.\n\n%s"
                              state doc))
        :modes (defvar ,modes nil
                 ,(format "Modes that should come up in %s state."
                          state))
        :predicate ',predicate
        :enable ',enable)

       ,@(when suppress-keymap
           `((set-keymap-parent ,keymap evil-suppress-map)))

       (dolist (func ',entry-hook-value)
         (add-hook ',entry-hook func))

       (dolist (func ',exit-hook-value)
         (add-hook ',exit-hook func))

       (defun ,predicate (&optional state)
         ,(format "Whether the current STATE is %s." state)
         (eq (or state evil-state) ',state))

       ;; define state function
       (evil-define-command ,toggle (&optional arg)
         :keep-visual t
         ,(format "Enable %s state. Disable with negative ARG.
If ARG is nil, don't display a message in the echo area.\n\n%s"
                  state doc)
         (interactive "p")
         (cond
          ((and (numberp arg) (< arg 1))
           (setq evil-previous-state evil-state
                 evil-state nil)
           (let ((evil-state ',state))
             (run-hooks ',exit-hook)
             (setq evil-state nil)
             (evil-normalize-keymaps)
             ,@body))
          (t
           (unless evil-local-mode
             (evil-initialize))
           (evil-change-state nil)
           (setq evil-state ',state)
           (let ((evil-state ',state)
                 (evil-next-state ',state))
             (evil-normalize-keymaps)
             (unless evil-locked-display
               (evil-refresh-cursor ',state)
               (setq evil-modeline-tag ,tag)
               (force-mode-line-update)
               (when (evil-called-interactively-p)
                 (redisplay)))
             ,@body
             (run-hooks ',entry-hook)
             (when (and arg (not evil-locked-display) ,message)
               (if (functionp ,message)
                   (funcall ,message)
                 (evil-echo ,message)))))))

       (evil-define-keymap ,keymap nil
         :mode ,mode)

       (evil-define-keymap ,local-keymap nil
         :mode ,local
         :local t)

       ',state)))

;;; Define Normal state and Emacs state

(evil-define-state normal
  "Normal state, AKA \"Command\" state."
  :tag " <N> "
  :enable (motion)
  :exit-hook (evil-repeat-start-hook)
  (cond
   ((evil-normal-state-p)
    (add-hook 'post-command-hook 'evil-normal-post-command nil t))
   (t
    (remove-hook 'post-command-hook 'evil-normal-post-command t))))

(defun evil-normal-post-command ()
  "Prevent point from reaching the end of the line."
  (when (evil-normal-state-p)
    (setq evil-this-type nil
          evil-this-operator nil
          evil-this-motion nil
          evil-this-motion-count nil
          evil-inhibit-operator nil
          evil-inhibit-operator-value nil)
    (unless (eq this-command 'evil-use-register)
      (setq evil-this-register nil))
    (evil-adjust-eol)
    (when (region-active-p)
      (and (fboundp 'evil-visual-state)
           (evil-visual-state)))))

(evil-define-state emacs
  "Emacs state."
  :tag " <E> "
  :message "-- EMACS --")

(provide 'evil-states)

;;; evil-states.el ends here
