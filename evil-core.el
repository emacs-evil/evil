;;;; Core functionality

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
    ;; restore the proper value of `major-mode' in Fundamental buffers
    (when (eq major-mode 'evil-local-mode)
      (setq major-mode 'fundamental-mode))
    ;; determine and enable the initial state
    (evil-initialize-state)
    ;; re-determine the initial state in `post-command-hook' since the
    ;; major mode may not be initialized yet, and some modes neglect
    ;; to run `after-change-major-mode-hook'
    (add-hook 'input-method-activate-hook 'evil-activate-input-method t t)
    (add-hook 'input-method-inactivate-hook 'evil-inactivate-input-method t t)
    (add-hook 'post-command-hook 'evil-initialize-state t t)
    (add-hook 'pre-command-hook 'evil-repeat-pre-hook)
    (add-hook 'post-command-hook 'evil-repeat-post-hook)
    (add-hook 'post-command-hook 'evil-refresh-cursor))
   (t
    (evil-refresh-mode-line)
    (remove-hook 'input-method-activate-hook 'evil-activate-input-method t)
    (remove-hook 'input-method-inactivate-hook 'evil-inactivate-input-method t)
    (evil-change-state nil))))

(defun evil-initialize ()
  "Enable Evil in the current buffer, if appropriate.
To enable Evil globally, do (evil-mode 1)."
  ;; TODO: option for enabling vi keys in the minibuffer
  (unless (minibufferp)
    (evil-local-mode 1)
    (remove-hook 'post-command-hook 'evil-initialize-state t)))

;;;###autoload (autoload 'evil-mode "evil")
(define-globalized-minor-mode evil-mode
  evil-local-mode evil-initialize)

;; to ensure that Fundamental buffers come up in Normal state,
;; initialize `fundamental-mode' via `evil-local-mode'
(defadvice evil-mode (after start-evil activate)
  "Enable Evil in Fundamental mode."
  (if evil-mode
      (progn
        ;; this is changed back when initializing `evil-local-mode'
        (setq-default major-mode 'evil-local-mode)
        (ad-enable-regexp "^evil")
        (ad-activate-regexp "^evil"))
    (setq-default major-mode 'fundamental-mode)
    (ad-disable-regexp "^evil")
    (ad-update-regexp "^evil")))

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

(evil-define-command evil-change-to-initial-state
  (&optional buffer message)
  "Change state to the initial state for BUFFER.
This is the state the buffer comes up in."
  :keep-visual t
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (evil-change-state (evil-initial-state-for-buffer
                        buffer (or evil-default-state 'normal))
                       message)))

(evil-define-command evil-change-to-previous-state
  (&optional buffer message)
  "Change the state of BUFFER to its previous state."
  :keep-visual t
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (evil-change-state (or evil-previous-state evil-default-state 'normal)
                       message)))

(evil-define-command evil-exit-emacs-state (&optional buffer message)
  "Change from Emacs state to the previous state."
  :keep-visual t
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
               (or message (not (eq state evil-state))))
      (funcall func (if state (and message 1) -1)))))

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
          (dolist (entry (evil-state-property t :modes))
            (setq state (car entry)
                  modes (symbol-value (cdr entry)))
            (when (memq mode modes)
              (throw 'loop state))))
        default)))

(defun evil-set-initial-state (mode state)
  "Set the initial state for MODE to STATE.
This is the state the buffer comes up in."
  (dolist (modes (evil-state-property t :modes))
    (setq modes (cdr-safe modes))
    (set modes (delq mode (symbol-value modes))))
  (when state
    (add-to-list (evil-state-property state :modes) mode)))

(defadvice display-buffer (before evil activate)
  "Initialize Evil in the displayed buffer."
  (when evil-mode
    (with-current-buffer (ad-get-arg 0)
      (unless evil-local-mode
        (evil-local-mode 1)
        (evil-initialize-state)))))

(defun evil-refresh-mode-line (&optional state)
  "Refresh mode line tag."
  (let (name next string temp)
    (setq string (symbol-value (evil-state-property state :tag))
          name (evil-state-property state :name))
    ;; add tooltip
    (when (stringp string)
      (setq string
            (propertize string
                        'help-echo name
                        'mouse-face 'mode-line-highlight)))
    (setq evil-mode-line-tag string)
    ;; refresh mode line data structure
    (when (or (null evil-local-mode)
              (null state)
              (not (eq evil-mode-line-format 'before)))
      (setq mode-line-position
            (delq 'evil-mode-line-tag mode-line-position)))
    (when (or (null evil-local-mode)
              (null state)
              (not (eq evil-mode-line-format 'after)))
      (while global-mode-string
        (setq next (pop global-mode-string))
        (if (eq next 'evil-mode-line-tag)
            (pop temp) ; remove the ""
          (push next temp)))
      (setq global-mode-string (nreverse temp)))
    (when evil-local-mode
      (when (eq evil-mode-line-format 'before)
        (add-to-list 'mode-line-position 'evil-mode-line-tag t 'eq))
      (when (eq evil-mode-line-format 'after)
        (unless (memq 'evil-mode-line-tag global-mode-string)
          (setq global-mode-string
                (nconc global-mode-string '("" evil-mode-line-tag))))))
    (force-mode-line-update)))

(defun evil-activate-input-method ()
  "Disable input method in states with :input-method nil."
  (let (input-method-activate-hook
        input-method-inactivate-hook)
    (when (and evil-local-mode evil-state)
      (setq evil-input-method current-input-method)
      (unless (evil-state-property evil-state :input-method)
        (inactivate-input-method)))))

(defun evil-inactivate-input-method ()
  "Disable input method in states with :input-method nil."
  (let (input-method-activate-hook
        input-method-inactivate-hook)
    (when (and evil-local-mode evil-state)
      (setq evil-input-method nil))))

(defadvice toggle-input-method (around evil activate)
  "Refresh `evil-input-method'."
  (cond
   ((not evil-local-mode)
    ad-do-it)
   ((evil-state-property evil-state :input-method)
    ad-do-it)
   (t
    (let ((current-input-method evil-input-method))
      ad-do-it))))

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

(defun evil-make-overriding-map (keymap &optional state copy)
  "Give KEYMAP precedence over the global keymap of STATE.
The keymap will have lower precedence than custom STATE bindings.
If STATE is nil, give it precedence over all states.
If COPY is t, create a copy of KEYMAP and give that
higher precedence. See also `evil-make-intercept-map'."
  (let ((key [override-state]))
    (when (and copy (not (keymapp copy)))
      (setq copy (assq-delete-all 'menu-bar (copy-keymap keymap))))
    (cond
     ((keymapp copy)
      (define-key copy key (or state 'all))
      (define-key keymap key copy))
     (t
      (define-key keymap key (or state 'all))))))

(defun evil-make-intercept-map (keymap &optional state)
  "Give KEYMAP precedence over all Evil keymaps in STATE.
If STATE is nil, give it precedence over all states.
See also `evil-make-overriding-map'."
  (let ((key [intercept-state]))
    (define-key keymap key (or state 'all))))

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
  (let ((func t)
        arg intercept key local mode overriding)
    (while (keywordp (car-safe body))
      (setq key (pop body)
            arg (pop body))
      (cond
       ((eq key :mode)
        (setq mode arg))
       ((eq key :local)
        (setq local arg))
       ((eq key :func)
        (setq func arg))
       ((eq key :intercept)
        (setq intercept arg))
       ((eq key :overriding)
        (setq overriding arg))))
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
       (when ,intercept
         (evil-make-intercept-map ,keymap))
       (when ,overriding
         (evil-make-overriding-map ,keymap))
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

;; Intercept the ESC event when running in the terminal. This allows
;; keys that use "ESC" as a prefix key, such as "M-x". If "ESC" is
;; immediately followed by another key, or another key is pressed
;; within `evil-esc-delay', the prefixed key sequence is sent.
;; Otherwise only [escape] is sent.
(evil-define-keymap evil-esc-map
  "Keymap for intercepting ESC."
  :intercept t)

(defun evil-turn-on-esc-mode ()
  "Enable interception of ESC."
  (unless (eq this-command 'evil-esc)
    (evil-esc-mode 1)
    (remove-hook 'pre-command-hook 'evil-turn-on-esc-mode t)))

;; TODO: this will probably not work well with the repeat-system.
(evil-define-command evil-esc (arg)
  "Wait for further keys within `evil-esc-delay'.
Otherwise send [escape]."
  :repeat ignore
  (interactive "P")
  (if (sit-for evil-esc-delay t)
      (push 'escape unread-command-events)
    (push last-command-event unread-command-events)
    ;; preserve prefix argument
    (setq prefix-arg arg))
  ;; disable interception for the next key sequence
  (evil-esc-mode -1)
  (add-hook 'pre-command-hook 'evil-turn-on-esc-mode nil t))

;; `evil-esc' is bound to (kbd "ESC"), while other commands
;; are bound to [escape]. That way `evil-esc' is used only when
;; (kbd "ESC") and [escape] are the same event -- i.e., when
;; running Emacs in the terminal.
(define-key evil-esc-map (kbd "ESC") 'evil-esc)

(defun evil-state-keymaps (state &rest excluded)
  "Return an ordered list of keymaps activated by STATE.
Skip states listed in EXCLUDED."
  (let* ((state (or state evil-state))
         (map (symbol-value (evil-state-property state :keymap)))
         (local-map (symbol-value (evil-state-property
                                   state :local-keymap)))
         (aux-maps (evil-state-auxiliary-keymaps state))
         (overriding-maps (evil-state-overriding-keymaps state))
         (enable (evil-state-property state :enable))
         (result (evil-state-intercept-keymaps state)))
    (add-to-list 'enable state)
    ;; the keymaps for other states and modes enabled by STATE
    (dolist (entry enable result)
      (cond
       ((memq entry excluded))
       ((eq entry state)
        (setq result
              (evil-concat-lists
               result
               (list local-map)
               aux-maps
               overriding-maps
               (list map)))
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
    (dolist (mode (append evil-mode-map-alist
                          evil-local-keymaps-alist))
      (setq mode (car-safe mode))
      (when (and (fboundp mode) (symbol-value mode))
        (funcall mode -1))
      (set mode nil))
    ;; enable modes for current state
    (when state
      (dolist (map (evil-state-keymaps state))
        (if (or (evil-auxiliary-keymap-p map)
                (evil-overriding-keymap-p map))
            (add-to-list 'alist (cons t map) t 'eq)
          (when (setq mode (or (evil-keymap-mode map) t))
            (when (and (fboundp mode) (null (symbol-value mode)))
              (funcall mode 1))
            (unless (eq mode t)
              (set mode t))
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

(defun evil-keymap-mode (keymap)
  "Return minor mode for KEYMAP.
See also `evil-mode-keymap'."
  (let ((map (if (keymapp keymap) keymap (symbol-value keymap)))
        (var (when (symbolp keymap) keymap)))
    ;; Check Evil variables first for speed purposes.
    ;; If all else fails, check `minor-mode-map-alist'.
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
  (let ((state (or state evil-state))
        aux result)
    (dolist (map (current-active-maps) result)
      (when (setq aux (evil-get-auxiliary-keymap map state))
        (add-to-list 'result aux t 'eq)))))

(defun evil-state-overriding-keymaps (&optional state)
  "Return an ordered list of overriding keymaps for STATE."
  (let* ((state (or state evil-state))
         result)
    (dolist (map (current-active-maps))
      (when (setq map (evil-overriding-keymap-p map state))
        (push map result)))
    (nreverse result)))

(defun evil-state-intercept-keymaps (&optional state)
  "Return an ordered list of intercept keymaps for STATE."
  (let* ((state (or state evil-state))
         (result (list evil-esc-map)))
    (dolist (map (current-active-maps))
      (when (setq map (evil-intercept-keymap-p map state))
        (push map result)))
    (nreverse result)))

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
           (aux (if state (lookup-key map key) map)))
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

(defun evil-intercept-keymap-p (map &optional state)
  "Whether MAP is an intercept keymap for STATE.
If STATE is nil, it means any state."
  (let ((entry (and (keymapp map)
                    (lookup-key map [intercept-state]))))
    (cond
     ((null entry)
      nil)
     ((null state)
      map)
     ((eq entry state)
      map)
     ((eq entry 'all)
      map))))

(defun evil-overriding-keymap-p (map &optional state)
  "Whether MAP is an overriding keymap for STATE.
If STATE is nil, it means any state."
  (let ((entry (and (keymapp map)
                    (lookup-key map [override-state]))))
    (cond
     ((null entry)
      nil)
     ((keymapp entry)
      (evil-overriding-keymap-p entry state))
     ((null state)
      map)
     ((eq entry state)
      map)
     ((eq entry 'all)
      map))))

(defun evil-define-key (state keymap key def &rest bindings)
  "Create a STATE binding from KEY to DEF for KEYMAP.
STATE is one of `normal', `insert', `visual', `replace',
`operator', `motion' and `emacs'. The remaining arguments
are like those of `define-key'. For example:

    (evil-define-key 'normal foo-map \"a\" 'bar)

This creates a binding from \"a\" to `bar' in Normal state,
which is active whenever `foo-map' is active. It is possible
to specify multiple bindings at once:

    (evil-define-key 'normal foo-map
      \"a\" 'bar
      \"b\" 'foo)

See also `evil-declare-key'."
  (let ((aux (if state
                 (evil-get-auxiliary-keymap keymap state t)
               keymap)))
    (while key
      (define-key aux key def)
      (setq key (pop bindings)
            def (pop bindings)))
    ;; ensure the prompt string comes first
    (evil-set-keymap-prompt aux (keymap-prompt aux))))

(defmacro evil-declare-key (state keymap key def &rest bindings)
  "Declare a STATE binding from KEY to DEF in KEYMAP.
Similar to `evil-define-key', but also works if KEYMAP is unbound;
the execution is postponed until KEYMAP is bound. For example:

    (evil-declare-key 'normal foo-map \"a\" 'bar)

The arguments are exactly like those of `evil-define-key',
and should be quoted as such."
  (declare (indent defun))
  (let ((func (evil-generate-symbol)))
    `(let (package)
       (cond
        ((boundp ',keymap)
         (evil-define-key ,state ,keymap ,key ,def ,@bindings))
        ((setq package
               (or (cdr-safe (assq ',keymap evil-overriding-maps))
                   (cdr-safe (assq ',keymap evil-intercept-maps))))
         (eval-after-load package
           '(evil-define-key ,state ,keymap ,key ,def ,@bindings)))
        (t
         (defun ,func (&rest args)
           (when (boundp ',keymap)
             (unless (keymapp ,keymap)
               (setq ,keymap (make-sparse-keymap)))
             (evil-define-key ,state ,keymap ,key ,def ,@bindings)
             (remove-hook 'after-load-functions ',func)))
         (add-hook 'after-load-functions ',func))))))

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

;; The following functions may enable an overriding keymap
;; or a keymap with state bindings. Therefore we need to
;; refresh `evil-mode-map-alist'.
(defadvice use-global-map (after evil activate)
  "Refresh Evil keymaps."
  (evil-normalize-keymaps))

(defadvice use-local-map (after evil activate)
  "Refresh Evil keymaps."
  (evil-normalize-keymaps))

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
  (let* ((name (and (string-match "^\\(.+\\)\\(\\(?:.\\|\n\\)*\\)" doc)
                    (match-string 1 doc)))
         (doc (match-string 2 doc))
         (name (and (string-match "^\\(.+?\\)\\.?$" name)
                    (match-string 1 name)))
         (doc (if (or (null doc) (string= doc "")) ""
                (format "\n%s" doc)))
         (toggle (intern (format "evil-%s-state" state)))
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
         input-method key message-value suppress-keymap tag-value)
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
       ((eq key :input-method)
        (setq input-method arg))
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
        :name ',name
        :toggle ',toggle
        :mode (defvar ,mode nil
                ,(format "Non-nil if %s is enabled.
Use the command `%s' to change this variable." name toggle))
        :keymap (defvar ,keymap (make-sparse-keymap)
                  ,(format "Keymap for %s." name))
        :local (defvar ,local nil
                 ,(format "Non-nil if %s is enabled.
Use the command `%s' to change this variable." name toggle))
        :local-keymap (defvar ,local-keymap nil
                        ,(format "Buffer-local keymap for %s." name))
        :tag (defvar ,tag ,tag-value
               ,(format "Mode line tag for %s." name))
        :message (defvar ,message ,message-value
                   ,(format "Echo area indicator for %s." name))
        :cursor (defvar ,cursor ',cursor-value
                  ,(format "Cursor for %s.
May be a cursor type as per `cursor-type', a color string as passed
to `set-cursor-color', a zero-argument function for changing the
cursor, or a list of the above." name))
        :entry-hook (defvar ,entry-hook nil
                      ,(format "Hooks to run when entering %s." name))
        :exit-hook (defvar ,exit-hook nil
                     ,(format "Hooks to run when exiting %s." name))
        :modes (defvar ,modes nil
                 ,(format "Modes that should come up in %s." name))
        :input-method ',input-method
        :predicate ',predicate
        :enable ',enable)

       ,@(when suppress-keymap
           `((set-keymap-parent ,keymap evil-suppress-map)))

       (dolist (func ',entry-hook-value)
         (add-hook ',entry-hook func))

       (dolist (func ',exit-hook-value)
         (add-hook ',exit-hook func))

       (defun ,predicate (&optional state)
         ,(format "Whether the current state is %s.
\(That is, whether `evil-state' is `%s'.)" name state)
         (and evil-local-mode
              (eq (or state evil-state) ',state)))

       ;; define state function
       (evil-define-command ,toggle (&optional arg)
         :keep-visual t
         ,(format "Enable %s. Disable with negative ARG.
If ARG is nil, don't display a message in the echo area.%s" name doc)
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
           (let ((evil-next-state ',state)
                 input-method-activate-hook
                 input-method-inactivate-hook)
             (evil-change-state nil)
             (setq evil-state ',state)
             (let ((evil-state ',state))
               (evil-normalize-keymaps)
               (if ',input-method
                   (activate-input-method evil-input-method)
                 (inactivate-input-method))
               (unless evil-locked-display
                 (evil-refresh-cursor ',state)
                 (evil-refresh-mode-line ',state)
                 (when (evil-called-interactively-p)
                   (redisplay)))
               ,@body
               (run-hooks ',entry-hook)
               (when (and evil-echo-state
                          arg (not evil-locked-display) ,message)
                 (if (functionp ,message)
                     (funcall ,message)
                   (evil-echo ,message))))))))

       (evil-define-keymap ,keymap nil
         :mode ,mode
         :func nil)

       (evil-define-keymap ,local-keymap nil
         :mode ,local
         :local t
         :func nil)

       ',state)))

;;; Define Normal state and Emacs state

(evil-define-state normal
  "Normal state.
AKA \"Command\" state."
  :tag " <N> "
  :enable (motion)
  :exit-hook (evil-repeat-start-hook)
  (cond
   ((evil-normal-state-p)
    (add-hook 'post-command-hook 'evil-normal-post-command nil t))
   (t
    (remove-hook 'post-command-hook 'evil-normal-post-command t))))

(defun evil-normal-post-command ()
  "Reset command loop variables in Normal state.
Also prevent point from reaching the end of the line.
If the region is activated, enter Visual state."
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
  :message "-- EMACS --"
  :input-method t
  (evil-esc-mode -1))

(provide 'evil-core)

;;; evil-core.el ends here
