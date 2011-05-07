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
(require 'evil-compatibility)

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
          (evil-concat-alists '(evil-mode-map-alist)
                              emulation-mode-map-alists))
    (evil-refresh-local-keymaps)
    (unless (memq 'evil-modeline-tag global-mode-string)
      (setq global-mode-string
            (append '("" evil-modeline-tag)
                    global-mode-string)))
    (ad-enable-advice 'show-paren-function 'around 'evil-show-paren-function)
    (ad-activate 'show-paren-function)
    (evil-normal-state))
   (t
    (let (new-global-mode-string)
      (while global-mode-string
        (let ((next (pop global-mode-string)))
          (if (eq next 'evil-modeline-tag)
              (pop new-global-mode-string) ;; remove the ""
            (push next new-global-mode-string))))
      (setq global-mode-string (nreverse new-global-mode-string)))
    (ad-disable-advice 'show-paren-function 'around 'evil-show-paren-function)
    (ad-activate 'show-paren-function)
    (evil-change-state nil))))

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

(defun evil-change-state (state)
  "Change state to STATE.
Disable all states if nil."
  (let ((func (evil-state-property (or state evil-state) :mode)))
    (when (and (functionp func)
               (not (eq state evil-state)))
      (funcall func (if state 1 -1)))))

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
       ((memq entry excluded)
        nil)
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
        (modes (evil-concat-lists
                (mapcar 'cdr (evil-state-property nil :mode))
                (mapcar 'cdr (evil-state-property nil :local-mode))))
        alist mode)
    (evil-refresh-global-keymaps)
    (evil-refresh-local-keymaps)
    ;; disable all modes
    (dolist (mode (mapcar 'car (append evil-mode-map-alist
                                       evil-local-keymaps-alist)))
      ;; modes not defined by a state are disabled
      ;; with their toggle function (if any)
      (when (and (fboundp mode)
                 (not (memq mode modes)))
        (funcall mode -1))
      (set mode nil))
    ;; enable modes for current state
    (when state
      (dolist (map (evil-state-keymaps state))
        (if (evil-auxiliary-keymap-p map)
            (evil-add-to-alist 'alist t map)
          (when (setq mode (evil-keymap-mode map))
            ;; enable non-state modes
            (when (and (fboundp mode)
                       (not (memq mode modes)))
              (funcall mode 1))
            (set mode t)
            ;; refresh the keymap in case it has changed
            ;; (e.g., `evil-operator-shortcut-map' is
            ;; reset on toggling)
            (setq map (or (evil-mode-keymap mode) map))
            (evil-add-to-alist 'alist mode map)))))
    ;; move the enabled modes to the front of the list
    (setq evil-mode-map-alist
          (evil-concat-alists
           alist evil-mode-map-alist))))

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
         (alist (symbol-value (evil-state-property state :aux)))
         aux result)
    (dolist (map (current-active-maps) result)
      (when (setq aux (evil-get-auxiliary-keymap map state))
        (add-to-list 'result aux t 'eq)))))

(defun evil-set-auxiliary-keymap (map state &optional aux)
  "Set the auxiliary keymap for MAP in STATE to AUX.
If AUX is nil, create a new auxiliary keymap."
  (unless (keymapp aux)
    (setq aux (make-sparse-keymap
               (format "Auxiliary keymap for %s state" state))))
  (define-key map
    (vconcat (list (intern (format "%s-state" state)))) aux)
  aux)

(defun evil-get-auxiliary-keymap (map state)
  "Get the auxiliary keymap for MAP in STATE."
  (lookup-key map (vconcat (list (intern (format "%s-state" state))))))

(defun evil-auxiliary-keymap-p (map)
  "Whether MAP is an auxiliary keymap."
  (and (keymapp map)
       (string-match "Auxiliary keymap" (or (keymap-prompt map) "")) t))

(defun evil-define-key (state keymap key def)
  "Create a STATE binding from KEY to DEF for KEYMAP.
The syntax is equivalent to that of `define-key'. For example:

    (evil-define-key 'normal text-mode-map \"a\" 'foo)

This will create a binding from \"a\" to `foo' in Normal state,
which will be active whenever `text-mode-map' is active."
  (let ((aux (if state
                 (or (evil-get-auxiliary-keymap keymap state)
                     (evil-set-auxiliary-keymap keymap state))
               keymap)))
    (define-key-after aux key def)))

(put 'evil-define-key 'lisp-indent-function 'defun)

;; these may be useful for programmatic purposes
(defun evil-global-set-key (state key def)
  "Bind KEY to DEF in STATE."
  (define-key (symbol-value (evil-state-property state :keymap))
    key def))

(defun evil-local-set-key (state key def)
  "Bind KEY to DEF in STATE in the current buffer."
  (define-key (symbol-value (evil-state-property state :local-keymap))
    key def))

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
  (let* ((mode (intern (format "evil-%s-state" state)))
         (keymap (intern (format "%s-map" mode)))
         (local-mode (intern (format "%s-local" mode)))
         (local-keymap (intern (format "%s-local-map" mode)))
         (aux (intern (format "%s-auxiliary-maps" mode)))
         (predicate (intern (format "%s-p" mode)))
         (tag (intern (format "%s-tag" mode)))
         (message (intern (format "%s-message" mode)))
         (cursor (intern (format "%s-cursor" mode)))
         (entry-hook (intern (format "%s-entry-hook" mode)))
         (exit-hook (intern (format "%s-exit-hook" mode)))
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
        (setq suppress-keymap arg))
       (t nil)))

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
        :entry-hook (defvar ,entry-hook nil
                      ,(format "Hooks to run when entering %s state.\n\n%s"
                               state doc))
        :exit-hook (defvar ,exit-hook nil
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
        :predicate ',predicate
        :enable ',enable)

       ,@(when suppress-keymap
           `((set-keymap-parent ,keymap evil-suppress-map)))

       (dolist (func ',entry-hook-value)
         (add-hook ',entry-hook func))

       (dolist (func ',exit-hook-value)
         (add-hook ',exit-hook func))

       (defun ,predicate ()
         ,(format "Whether the current state is %s." state)
         (eq evil-state ',state))

       ;; define state function
       (defun ,mode (&optional arg)
         ,(format "Enable %s state. Disable with negative ARG.\n\n%s"
                  state doc)
         (interactive "p")
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
           (evil-change-state nil)
           (unwind-protect
               (let ((evil-state ',state))
                 (evil-normalize-keymaps)
                 (setq evil-modeline-tag ,tag)
                 (force-mode-line-update)
                 (evil-set-cursor ,cursor)
                 ,@body
                 (run-hooks ',entry-hook)
                 (when (and arg ,message)
                   (if (functionp ,message)
                       (funcall ,message)
                     (evil-echo ,message))))
             (setq evil-state ',state)))))
       (evil-set-command-properties ',mode :keep-visual t)

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
  :enable (motion operator)
  (cond
   ((evil-normal-state-p)
    (evil-setup-normal-repeat)
    (add-hook 'post-command-hook 'evil-normal-post-command nil t))
   (t
    (unless evil-state
      (evil-teardown-normal-repeat))
    (remove-hook 'post-command-hook 'evil-normal-post-command t))))

(defun evil-normal-post-command ()
  "Prevent point from reaching the end of the line."
  (when (evil-normal-state-p)
    (when (and (eolp) (not (bolp)))
      (backward-char))))

(evil-define-state emacs
  "Emacs state."
  :tag " <E> "
  :message "-- EMACS --")

;; TODO: this function is not perfect: if (point) is placed behind a
;; closing parenthesis that pair will be highlighted even if
;; `evil-show-paren-range' is 0. The problem is to find a position not
;; adjacent to a parenthesis because otherwise the default-behaviour
;; of show-parent-function will apply.
(defadvice show-paren-function (around evil-show-paren-function)
  "Advices show-paren-function so also parentheses near point are matched."
  (save-excursion
    (goto-char
     (or (catch 'end
           (save-excursion
             (dotimes (d (1+ (* 2 evil-show-paren-range)))
               (forward-char (if (evenp d) d (- d)))
               (let ((sc (syntax-class (syntax-after (point)))))
                 (case sc
                   (4 (throw 'end (point)))
                   (5 (throw 'end (1+ (point)))))))
             nil))
         (point)))
    ad-do-it))

(provide 'evil-states)

;;; evil-states.el ends here
