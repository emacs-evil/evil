;;;; Core functionality

;; Evil is defined as a globalized minor mode, enabled with the toggle
;; function `evil-mode'. This in turn enables `evil-local-mode' in
;; every buffer, which sets up the buffer's state.
;;
;; Each state has its own keymaps, and these keymaps have status as
;; "emulation keymaps" with priority over regular keymaps. Emacs
;; maintains the following keymap hierarchy (highest priority first):
;;
;;     * Overriding keymaps/overlay keymaps...
;;     * Emulation mode keymaps...
;;       - Evil keymaps...
;;     * Minor mode keymaps...
;;     * Local keymap (`local-set-key')
;;     * Global keymap (`global-set-key')
;;
;; Within this hierarchy, Evil arranges the keymaps for the current
;; state as shown below:
;;
;;     * Intercept keymaps...
;;     * Local state keymap
;;     * Auxiliary keymaps...
;;     * Overriding keymaps...
;;     * Global state keymap
;;     * Keymaps for other states...
;;
;; These keymaps are listed in `evil-mode-map-alist', which is listed
;; in `emulation-mode-map-alist'.
;;
;; Most of the key bindings for a state are stored in its global
;; keymap, which has a name such as `evil-normal-state-map'. (See the
;; file evil-maps.el, which contains all the default key bindings.)
;; A state also has a local keymap (`evil-normal-state-local-map'),
;; which may contain user customizations for the current buffer.
;; Furthermore, any Emacs mode may be assigned state bindings of its
;; own by passing the mode's keymap to the function `evil-define-key'.
;; These mode-specific bindings are ultimately stored in so-called
;; auxiliary keymaps, which are sandwiched between the local keymap
;; and the global keymap. Finally, the state may also activate the
;; keymaps of other states (e.g., Normal state inherits bindings
;; from Motion state).
;;
;; For integration purposes, a regular Emacs keymap may be "elevated"
;; to emulation status by passing it to `evil-make-intercept-map' or
;; `evil-make-overriding-map'. An "intercept" keymap has priority over
;; all other Evil keymaps. (Evil uses this facility when debugging and
;; for handling the "ESC" key in the terminal.) More common is the
;; "overriding" keymap, which only has priority over the global state
;; keymap. (This is useful for adapting key-heavy modes such as Dired,
;; where all but a few keys should be left as-is and should not be
;; shadowed by Evil's default bindings.)
;;
;; States are defined with the macro `evil-define-state', which
;; creates a command for switching to the state. This command,
;; for example `evil-normal-state' for Normal state, performs
;; the following tasks:
;;
;;     * Setting `evil-state' to the new state.
;;     * Refreshing the keymaps in `evil-mode-map-alist'.
;;     * Updating the mode line.
;;       - Normal state depends on `evil-normal-state-tag'.
;;     * Adjusting the cursor's appearance.
;;       - Normal state depends on `evil-normal-state-cursor'.
;;     * Displaying a message in the echo area.
;;       - Normal state depends on `evil-normal-state-message'.
;;     * Running hooks.
;;       - Normal state runs `evil-normal-state-entry-hook' when
;;         entering, and `evil-normal-state-exit-hook' when exiting.
;;
;; The various properties of a state can be accessed through their
;; respective variables, or by passing a keyword and the state's name
;; to the `evil-state-property' function. Evil defines the states
;; Normal state ("normal"), Insert state ("insert"), Visual state
;; ("visual"), Replace state ("replace"), Operator-Pending state
;; ("operator"), Motion state ("motion") and Emacs state ("emacs").

(require 'evil-common)

(define-minor-mode evil-local-mode
  "Minor mode for setting up Evil in a single buffer."
  :init-value nil
  (cond
   (evil-local-mode
    (setq emulation-mode-map-alists
          (evil-concat-lists '(evil-mode-map-alist)
                             emulation-mode-map-alists))
    (evil-initialize-local-keymaps)
    ;; restore the proper value of `major-mode' in Fundamental buffers
    (when (eq major-mode 'evil-local-mode)
      (setq major-mode 'fundamental-mode))
    ;; determine and enable the initial state
    (unless evil-state
      (evil-initialize-state)
      ;; re-determine the initial state in `post-command-hook' since
      ;; the major mode may not have been initialized yet
      (add-hook 'post-command-hook #'evil-initialize-state t t))
    (add-hook 'input-method-activate-hook #'evil-activate-input-method t t)
    (add-hook 'input-method-inactivate-hook #'evil-inactivate-input-method t t)
    (add-hook 'pre-command-hook #'evil-repeat-pre-hook)
    (add-hook 'post-command-hook #'evil-repeat-post-hook)
    (add-hook 'post-command-hook #'evil-refresh-cursor))
   (t
    (evil-refresh-mode-line)
    (remove-hook 'input-method-activate-hook #'evil-activate-input-method t)
    (remove-hook 'input-method-inactivate-hook #'evil-inactivate-input-method t)
    (evil-change-state nil))))

(defun evil-initialize ()
  "Enable Evil in the current buffer, if appropriate.
To enable Evil globally, do (evil-mode 1)."
  ;; TODO: option for enabling vi keys in the minibuffer
  (unless (minibufferp)
    (evil-local-mode 1)))

;;;###autoload (autoload 'evil-mode "evil")
(define-globalized-minor-mode evil-mode
  evil-local-mode evil-initialize)

;; No hooks are run in Fundamental buffers, so other measures are
;; necessary to initialize Evil in these buffers. When Evil is
;; enabled globally, the default value of `major-mode' is set to
;; `evil-local-mode', so that this function is called in Fundamental
;; buffers as well. Then, the buffer-local value of `major-mode' is
;; changed back to `fundamental-mode'. (Since the `evil-mode' function
;; is created by a macro, we use `defadvice' to augment it.)
(defadvice evil-mode (after start-evil activate)
  "Enable Evil in Fundamental mode."
  (if evil-mode
      (progn
        ;; changed back by `evil-local-mode'
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

(defun evil-change-state (state &optional message)
  "Change the state to STATE.
If STATE is nil, disable all states."
  (let ((func (evil-state-property (or state evil-state) :toggle)))
    (when (and (functionp func)
               (or message (not (eq state evil-state))))
      (funcall func (if state (and message 1) -1)))))

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
  `(evil-without-display
     (evil-save-state
       (evil-change-state ',state)
       ,@body)))

(defun evil-initializing-p (&optional buffer)
  "Whether Evil is in the process of being initialized."
  (with-current-buffer (or buffer (current-buffer))
    (memq #'evil-initialize-state post-command-hook)))

(defun evil-initialize-state (&optional state buffer)
  "Set up the initial state for BUFFER.
BUFFER defaults to the current buffer.
Uses STATE if specified, or calls `evil-initial-state-for-buffer'.
See also `evil-set-initial-state'."
  (with-current-buffer (or buffer (current-buffer))
    (remove-hook 'post-command-hook #'evil-initialize-state t)
    (if state (evil-change-state state)
      (evil-change-to-initial-state buffer))))
(put 'evil-initialize-state 'permanent-local-hook t)

(defun evil-initial-state-for-buffer (&optional buffer default)
  "Return the initial Evil state to use for BUFFER.
BUFFER defaults to the current buffer. Returns DEFAULT
if no initial state is associated with BUFFER.
See also `evil-initial-state'."
  (with-current-buffer (or buffer (current-buffer))
    (or (catch 'done
          (dolist (mode minor-mode-map-alist)
            (setq mode (car-safe mode))
            (when (and (boundp mode) (symbol-value mode))
              (when (setq mode (evil-initial-state mode))
                (throw 'done mode)))))
        (evil-initial-state major-mode)
        default)))

(defun evil-initial-state (mode &optional default)
  "Return the Evil state to use for MODE.
Returns DEFAULT if no initial state is associated with MODE.
The initial state for a mode can be set with
`evil-set-initial-state'."
  (let (state modes)
    (or (catch 'done
          (dolist (entry (evil-state-property t :modes))
            (setq state (car entry)
                  modes (symbol-value (cdr entry)))
            (when (memq mode modes)
              (throw 'done state))))
        default)))

(defun evil-set-initial-state (mode state)
  "Set the initial state for MODE to STATE.
This is the state the buffer comes up in."
  (dolist (modes (evil-state-property t :modes))
    (setq modes (cdr-safe modes))
    (set modes (delq mode (symbol-value modes))))
  (when state
    (add-to-list (evil-state-property state :modes) mode)))

(evil-define-command evil-change-to-initial-state
  (&optional buffer message)
  "Change the state of BUFFER to its initial state.
This is the state the buffer came up in."
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
  :repeat abort
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (evil-change-state (or evil-previous-state evil-default-state 'normal)
                       message)))

(defadvice display-buffer (before evil activate)
  "Initialize Evil in the displayed buffer."
  (when evil-mode
    (when (get-buffer (ad-get-arg 0))
      (with-current-buffer (ad-get-arg 0)
        (unless evil-local-mode
          (evil-local-mode 1))))))

(defadvice switch-to-buffer (before evil activate)
  "Initialize Evil in the displayed buffer."
  (when evil-mode
    (when (get-buffer (ad-get-arg 0))
      (with-current-buffer (ad-get-arg 0)
        (unless evil-local-mode
          (evil-local-mode 1))))))

(defun evil-refresh-mode-line (&optional state)
  "Refresh mode line tag."
  (let (name next string temp)
    (setq string (evil-state-property state :tag t)
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
        (add-to-list 'mode-line-position 'evil-mode-line-tag t #'eq))
      (when (eq evil-mode-line-format 'after)
        (unless (memq 'evil-mode-line-tag global-mode-string)
          (setq global-mode-string
                (nconc global-mode-string '("" evil-mode-line-tag))))))
    (force-mode-line-update)))

;; input methods should be disabled in non-insertion states
(defun evil-activate-input-method ()
  "Disable input method in states with :input-method nil."
  (let (input-method-activate-hook
        input-method-inactivate-hook)
    (when (and evil-local-mode evil-state)
      (setq evil-input-method current-input-method)
      (unless (evil-state-property evil-state :input-method)
        (inactivate-input-method)))))
(put 'evil-activate-input-method 'permanent-local-hook t)

(defun evil-inactivate-input-method ()
  "Disable input method in states with :input-method nil."
  (let (input-method-activate-hook
        input-method-inactivate-hook)
    (when (and evil-local-mode evil-state)
      (setq evil-input-method nil))))
(put 'evil-inactivate-input-method 'permanent-local-hook t)

;; When a buffer is created in a low-level way, it is invisible to
;; Evil (as well as other globalized minor modes) because no hooks are
;; run. This is appropriate since many buffers are used for throwaway
;; purposes. Passing the buffer to `display-buffer' indicates
;; otherwise, though, so advise this function to initialize Evil.
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

;; Local keymaps are implemented using buffer-local variables.
;; However, unless a buffer-local value already exists,
;; `define-key' acts on the variable's default (global) value.
;; So we need to initialize the variable whenever we enter a
;; new buffer or when the buffer-local values are reset.
(defun evil-initialize-local-keymaps ()
  "Initialize a buffer-local value for local keymaps as necessary.
The initial value is that of `make-sparse-keymap'."
  (dolist (entry evil-local-keymaps-alist)
    (let ((mode (car entry))
          (map  (cdr entry)))
      (unless (and (keymapp (symbol-value map))
                   (assq map (buffer-local-variables)))
        (set map (make-sparse-keymap))))))

(defun evil-make-overriding-map (keymap &optional state copy)
  "Give KEYMAP precedence over the global keymap of STATE.
The keymap will have lower precedence than custom STATE bindings.
If STATE is nil, give it precedence over all states.
If COPY is t, create a copy of KEYMAP and give that
higher precedence. See also `evil-make-intercept-map'."
  (let ((key [override-state]))
    (if (not copy)
        (define-key keymap key (or state 'all))
      (unless (keymapp copy)
        (setq copy (assq-delete-all 'menu-bar (copy-keymap keymap))))
      (define-key copy key (or state 'all))
      (define-key keymap key copy))))

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
       (put ',mode 'permanent-local t)
       (when ,intercept
         (evil-make-intercept-map ,keymap))
       (when ,overriding
         (evil-make-overriding-map ,keymap))
       ,@(if local
             `((make-variable-buffer-local ',keymap)
               (put ',keymap 'permanent-local t)
               (evil-add-to-alist 'evil-local-keymaps-alist
                                  ',mode ',keymap))
           `((evil-add-to-alist 'evil-global-keymaps-alist
                                ',mode ',keymap)
             (evil-add-to-alist 'evil-mode-map-alist
                                ',mode ,keymap)))
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
  (unless (eq this-command #'evil-esc)
    (evil-esc-mode 1)
    (remove-hook 'pre-command-hook #'evil-turn-on-esc-mode t)))
(put 'evil-turn-on-esc-mode 'permanent-local-hook t)

;; `evil-esc' is bound to (kbd "ESC"), while other commands
;; are bound to [escape]. That way `evil-esc' is used only when
;; (kbd "ESC") and [escape] are the same event -- i.e., when
;; running Emacs in the terminal.
(define-key evil-esc-map (kbd "ESC") 'evil-esc)

(defun evil-state-p (sym)
  "Whether SYM is the name of a state."
  (assq sym evil-state-properties))

(defun evil-state-keymaps (state &rest excluded)
  "Return a keymap alist of keymaps activated by STATE.
Recursively includes the keymaps of other states referenced by STATE.
The EXCLUDED argument is an internal safeguard against infinite
recursion, keeping track of earlier states."
  (let* ((state (or state evil-state))
         (enable (evil-state-property state :enable))
         (map (cons
               (evil-state-property state :mode)
               (evil-state-property state :keymap t)))
         (local-map (cons
                     (evil-state-property state :local)
                     (evil-state-property state :local-keymap t)))
         (aux-maps (evil-state-auxiliary-keymaps state))
         (overriding-maps
          (evil-state-overriding-keymaps state))
         (intercept-maps
          (evil-state-intercept-keymaps state))
         (result `(,intercept-maps))
         (remove-duplicates (null excluded)))
    (unless (memq state enable)
      (setq enable (cons state enable)))
    ;; process STATE's :enable property
    (dolist (entry enable)
      (cond
       ((memq entry excluded))
       ;; the keymaps for STATE
       ((eq entry state)
        (setq result `(,@result
                       (,local-map)
                       ,aux-maps
                       ,overriding-maps
                       (,map)))
        (push state excluded))
       ;; the keymaps for another state
       ((evil-state-p entry)
        (setq result `(,@result
                       ,(apply #'evil-state-keymaps entry excluded))))
       ;; a single keymap
       ((or (keymapp entry)
            (and (keymapp (symbol-value entry))
                 (setq entry (symbol-value entry)))
            (setq entry (evil-keymap-for-mode entry)))
        (setq result `(,@result
                       ((,(evil-mode-for-keymap entry t) .
                         ,entry)))))))
    ;; postpone the expensive filtering of duplicates to the top level
    (if remove-duplicates
        (apply #'evil-concat-keymap-alists result)
      (apply #'append result))))

(defun evil-normalize-keymaps (&optional state)
  "Create a buffer-local value for `evil-mode-map-alist'.
This is a keymap alist, determined by the current state
\(or by STATE if specified)."
  (let ((state (or state evil-state))
        (excluded '(nil t))
        map mode temp)
    ;; initialize buffer-local keymaps as necessary
    (evil-initialize-local-keymaps)
    ;; deactivate keymaps of previous state
    (dolist (entry evil-mode-map-alist)
      (setq mode (car-safe entry)
            map (cdr-safe entry))
      ;; overriding keymaps are not toggled here,
      ;; but by the mode they are associated with
      (if (or (memq mode excluded)
              (evil-intercept-keymap-p map)
              (evil-overriding-keymap-p map)
              (evil-auxiliary-keymap-p map))
          (push mode excluded)
        (when (and (fboundp mode) (symbol-value mode))
          (funcall mode -1))
        (set mode nil)))
    (setq evil-mode-map-alist nil)
    ;; activate keymaps of current state
    (when state
      (setq temp (evil-state-keymaps state))
      (dolist (entry temp)
        (setq mode (car entry)
              map (cdr entry))
        (unless (and (boundp mode) (symbol-value mode))
          (when (fboundp mode)
            (funcall mode 1))
          (set mode t))
        ;; refresh the keymap in case it has changed
        ;; (e.g., `evil-operator-shortcut-map' is
        ;; reset on toggling)
        (if (or (memq mode excluded)
                (evil-intercept-keymap-p map)
                (evil-overriding-keymap-p map)
                (evil-auxiliary-keymap-p map))
            (push mode excluded)
          (setcdr entry (or (evil-keymap-for-mode mode) map))))
      ;; update `evil-mode-map-alist'
      (setq evil-mode-map-alist temp))))

(defun evil-mode-for-keymap (keymap &optional default)
  "Return the minor mode associated with KEYMAP.
Returns DEFAULT if no mode is found.
See also `evil-keymap-for-mode'."
  (let ((map (if (keymapp keymap) keymap (symbol-value keymap)))
        (var (when (symbolp keymap) keymap)))
    ;; Check Evil variables first for speed purposes.
    ;; If all else fails, check `minor-mode-map-alist'.
    (or (when var
          (or (car (rassq var evil-global-keymaps-alist))
              (car (rassq var evil-local-keymaps-alist))))
        (car (rassq map (mapcar #'(lambda (e)
                                    ;; from (MODE-VAR . MAP-VAR)
                                    ;; to (MODE-VAR . MAP)
                                    (cons (car-safe e)
                                          (symbol-value (cdr-safe e))))
                                (append evil-global-keymaps-alist
                                        evil-local-keymaps-alist))))
        (car (rassq map minor-mode-map-alist))
        default)))

(defun evil-keymap-for-mode (mode &optional variable)
  "Return the keymap associated with MODE.
Return the keymap variable if VARIABLE is non-nil.
See also `evil-mode-for-keymap'."
  (let* ((var (or (cdr (assq mode evil-global-keymaps-alist))
                  (cdr (assq mode evil-local-keymaps-alist))))
         (map (or (symbol-value var)
                  (cdr (assq mode minor-mode-map-alist)))))
    (if variable var map)))

(defun evil-state-auxiliary-keymaps (state)
  "Return a keymap alist of auxiliary keymaps for STATE."
  (let ((state (or state evil-state))
        aux result)
    (dolist (map (current-active-maps) result)
      (when (setq aux (evil-get-auxiliary-keymap map state))
        (push (cons (evil-mode-for-keymap map t) aux) result)))
    (nreverse result)))

(defun evil-state-overriding-keymaps (&optional state)
  "Return a keymap alist of overriding keymaps for STATE."
  (let* ((state (or state evil-state))
         result)
    (dolist (map (current-active-maps))
      (when (setq map (evil-overriding-keymap-p map state))
        (push (cons (evil-mode-for-keymap map t) map) result)))
    (nreverse result)))

(defun evil-state-intercept-keymaps (&optional state)
  "Return a keymap alist of intercept keymaps for STATE."
  (let* ((state (or state evil-state))
         result)
    (dolist (map (current-active-maps))
      (when (setq map (evil-intercept-keymap-p map state))
        (push (cons (evil-mode-for-keymap map t) map) result)))
    (setq result (nreverse result))
    (add-to-list 'result `(evil-esc-mode . ,evil-esc-map))
    result))

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

(defun evil-intercept-keymap-state (map)
  "Return the state for the intercept keymap MAP.
A return value of t means all states."
  (let ((state (lookup-key map [intercept-state] map)))
    (cond
     ((keymapp state)
      (evil-intercept-keymap-state state))
     ((eq state 'all)
      t)
     (t
      state))))

(defun evil-overriding-keymap-state (map)
  "Return the state for the overriding keymap MAP.
A return value of t means all states."
  (let ((state (lookup-key map [override-state] map)))
    (cond
     ((keymapp state)
      (evil-overriding-keymap-state state))
     ((eq state 'all)
      t)
     (t
      state))))

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
             (remove-hook 'after-load-functions #',func)))
         (add-hook 'after-load-functions #',func t))))))

(defmacro evil-add-hjkl-bindings (keymap &optional state &rest bindings)
  "Add \"h\", \"j\", \"k\", \"l\" bindings to KEYMAP in STATE.
Add additional BINDINGS if specified."
  (declare (indent defun))
  `(evil-declare-key ,state ,keymap
     "h" (lookup-key evil-motion-state-map "h")
     "j" (lookup-key evil-motion-state-map "j")
     "k" (lookup-key evil-motion-state-map "k")
     "l" (lookup-key evil-motion-state-map "l")
     ,@bindings))

(put 'evil-define-key 'lisp-indent-function 'defun)
(put 'evil-set-auxiliary-keymap 'lisp-indent-function 'defun)

;; may be useful for programmatic purposes
(defun evil-global-set-key (state key def)
  "Bind KEY to DEF in STATE."
  (define-key (evil-state-property state :keymap t) key def))

(defun evil-local-set-key (state key def)
  "Bind KEY to DEF in STATE in the current buffer."
  (define-key (evil-state-property state :local-keymap t) key def))

;; Advise these functions as they may activate an overriding keymap or
;; a keymap with state bindings; if so, refresh `evil-mode-map-alist'.
(defadvice use-global-map (after evil activate)
  "Refresh Evil keymaps."
  (evil-normalize-keymaps))

(defadvice use-local-map (after evil activate)
  "Refresh Evil keymaps."
  (evil-normalize-keymaps))

(defmacro evil-define-state (state doc &rest body)
  "Define an Evil state STATE.
DOC is a general description and shows up in all docstrings;
the first line of the string should be the full name of the state.
Then follows one or more optional keywords:

:tag STRING             Mode line indicator.
:message STRING         Echo area message when changing to STATE.
:cursor SPEC            Cursor to use in STATE.
:entry-hook LIST        Hooks run when changing to STATE.
:exit-hook LIST         Hooks run when changing from STATE.
:enable LIST            List of other states and modes enabled by STATE.
:suppress-keymap FLAG   If FLAG is non-nil, makes `evil-suppress-map'
                        the parent of the global map of STATE,
                        effectively disabling bindings to
                        `self-insert-command'.

Following the keywords is optional code to be executed each time
the state is enabled or disabled. For example:

    (evil-define-state test
      \"Test state.\"
      :tag \"<T> \"
      (setq test-var t))

The global keymap of this state will be `evil-test-state-map',
the local keymap will be `evil-test-state-local-map', and so on.

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
         (intercept-esc t)
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
       ((eq key :intercept-esc)
        (setq intercept-esc arg))
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
                   ,(format "Echo area message for %s." name))
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
             (evil-esc-mode -1)
             (evil-normalize-keymaps)
             ,@body))
          (t
           (unless evil-local-mode
             (evil-local-mode 1)
             (evil-initialize-state))
           (let ((evil-next-state ',state)
                 input-method-activate-hook
                 input-method-inactivate-hook)
             (evil-change-state nil)
             (setq evil-state ',state)
             (let ((evil-state ',state))
               (evil-normalize-keymaps)
               (if ,intercept-esc
                   (evil-esc-mode 1)
                 (evil-esc-mode -1))
               (if ',input-method
                   (activate-input-method evil-input-method)
                 (inactivate-input-method))
               (unless evil-no-display
                 (evil-refresh-cursor ',state)
                 (evil-refresh-mode-line ',state)
                 (when (evil-called-interactively-p)
                   (redisplay)))
               ,@body
               (run-hooks ',entry-hook)
               (when (and evil-echo-state
                          arg (not evil-no-display) ,message)
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

(provide 'evil-core)

;;; evil-core.el ends here
