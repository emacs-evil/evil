;; evil-tests.el --- unit tests for Evil -*- coding: utf-8 -*-

;; This file is for developers. It runs some tests on Evil.
;; To load it, run the Makefile target "make test" or add
;; the following lines to .emacs:
;;
;;     (setq evil-tests-run nil) ; set to t to run tests immediately
;;     (global-set-key [f12] 'evil-tests-run) ; hotkey
;;     (require 'evil-tests)
;;
;; Loading this file enables profiling on Evil. The current numbers
;; can be displayed with `elp-results'. The Makefile target
;; "make profiler" shows profiling results in the terminal on the
;; basis of running all tests.
;;
;; To write a test, use `ert-deftest' and specify a :tags value of at
;; least '(evil). The test may inspect the output of functions given
;; certain input, or it may execute a key sequence in a temporary
;; buffer and investigate the results. For the latter approach, the
;; macro `evil-test-buffer' creates a temporary buffer in Normal
;; state. String descriptors initialize and match the contents of
;; the buffer:
;;
;;     (ert-deftest evil-test ()
;;       :tags '(evil)
;;       (evil-test-buffer
;;        "[T]his creates a test buffer." ; cursor on "T"
;;        ("w")                           ; key sequence
;;        "This [c]reates a test buffer."))) ; cursor moved to "c"
;;
;; The initial state, the cursor syntax, etc., can be changed
;; with keyword arguments. See the documentation string of
;; `evil-test-buffer' for more details.
;;
;; This file is NOT part of Evil itself.

(require 'elp)
(require 'ert)
(require 'evil)

(defvar evil-tests-run nil
  "*Run Evil tests.")

(defvar evil-tests-profiler nil
  "*Profile Evil tests.")

(defun evil-tests-initialize (&optional tests profiler interactive)
  (setq profiler (or profiler evil-tests-profiler))
  (when (listp profiler)
    (setq profiler (car profiler)))
  (when profiler
    (setq evil-tests-profiler t)
    (setq profiler
          (or (cdr (assq profiler
                         '((call . elp-sort-by-call-count)
                           (average . elp-sort-by-average-time)
                           (total . elp-sort-by-total-time))))))
    (setq elp-sort-by-function (or profiler 'elp-sort-by-call-count))
    (elp-instrument-package "evil"))
  (if interactive
      (if (y-or-n-p-with-timeout "Run tests? " 2 t)
          (evil-tests-run tests interactive)
        (message "You can run the tests at any time \
with `M-x evil-tests-run'"))
    (evil-tests-run tests)))

(defun evil-tests-run (&optional tests interactive)
  "Run Evil tests."
  (interactive '(nil t))
  (let ((elp-use-standard-output (not interactive)))
    (setq tests
          (or (null tests)
              `(or ,@(mapcar #'(lambda (test)
                                 (or (null test)
                                     (and (memq test '(evil t)) t)
                                     `(or (tag ,test)
                                          ,(format "^%s$" test))))
                             tests))))
    (cond
     (interactive
      (ert-run-tests-interactively tests)
      (when evil-tests-profiler
        (elp-results)))
     (evil-tests-profiler
      (ert-run-tests-batch tests)
      (elp-results))
     (t
      (ert-run-tests-batch-and-exit tests)))))

(defun evil-tests-profiler (&optional force)
  "Profile Evil tests."
  (when (or evil-tests-profiler force)
    (setq evil-tests-profiler t)
    (elp-instrument-package "evil")))

(defvar evil-test-point nil
  "Marker for point.")
(make-variable-buffer-local 'evil-test-point)
(defvar evil-test-visual-start nil
  "Marker for Visual beginning.")
(make-variable-buffer-local 'evil-test-visual-start)
(defvar evil-test-visual-end nil
  "Marker for Visual end.")
(make-variable-buffer-local 'evil-test-visual-end)

(defmacro evil-test-buffer (&rest body)
  "Execute FORMS in a temporary buffer.
The following optional keywords specify the buffer's properties:

:state STATE            The initial state, defaults to `normal'.
:visual TYPE            The Visual type, defaults to
                        `evil-visual-char'.
:point-start STRING     String for matching beginning of point,
                        defaults to \"[\".
:point-end STRING       String for matching end of point,
                        defaults to \"]\".
:visual-start STRING    String for matching beginning of
                        Visual selection, defaults to \"<\".
:visual-end STRING      String for matching end of
                        Visual selection, defaults to \">\".

Then follows one or more forms. If the first form is a string,
it is taken to be a buffer description as passed to
`evil-test-buffer-from-string', and initializes the buffer.
Subsequent string forms validate the buffer.

If a form is a list of strings or vectors, it is taken
to be a key sequence and is passed to `execute-kbd-macro'.
Remaining forms are evaluated as-is.

\(fn [[KEY VALUE]...] FORMS...)"
  (declare (indent defun))
  (let ((state 'normal)
        arg key point-start point-end string
        visual visual-start visual-end)
    ;; collect keywords
    (while (keywordp (car-safe body))
      (setq key (pop body)
            arg (pop body))
      (cond
       ((eq key :point-start)
        (setq point-start (or arg "")))
       ((eq key :point-end)
        (setq point-end (or arg "")))
       ((eq key :state)
        (setq state arg))
       ((eq key :visual)
        (setq visual arg))
       ((eq key :visual-start)
        (setq visual-start (or arg "")))
       ((eq key :visual-end)
        (setq visual-end (or arg "")))))
    ;; collect buffer initialization
    (when (stringp (car-safe body))
      (setq string (pop body)))
    ;; macro expansion
    `(let ((buffer (evil-test-buffer-from-string
                    ,string ',state
                    ,point-start ,point-end
                    ',visual ,visual-start ,visual-end))
           (kill-ring kill-ring)
           (kill-ring-yank-pointer kill-ring-yank-pointer)
           x-select-enable-clipboard
           message-log-max)
       (unwind-protect
           (save-window-excursion
             (with-current-buffer buffer
               ;; necessary for keyboard macros to work
               (switch-to-buffer-other-window (current-buffer))
               (buffer-enable-undo)
               ;; parse remaining forms
               ,@(mapcar
                  #'(lambda (form)
                      (cond
                       ((stringp form)
                        `(evil-test-buffer-string
                          ,form
                          ',point-start ',point-end
                          ',visual-start ',visual-end))
                       ((or (stringp (car-safe form))
                            (vectorp (car-safe form))
                            (memq (car-safe (car-safe form))
                                  '(kbd vconcat)))
                        ;; we need to execute everything as a single
                        ;; sequence for command loop hooks to work
                        `(execute-kbd-macro
                          (apply #'vconcat
                                 (mapcar #'listify-key-sequence
                                         (mapcar #'eval ',form)))))
                       ((memq (car-safe form) '(kbd vconcat))
                        `(execute-kbd-macro ,form))
                       (t
                        form)))
                  body)))
         (and (buffer-name buffer)
              (kill-buffer buffer))))))

(when (fboundp 'font-lock-add-keywords)
  (font-lock-add-keywords 'emacs-lisp-mode
                          '(("(\\(evil-test-buffer\\)\\>"
                             1 font-lock-keyword-face))))

(defun evil-test-buffer-string
  (string &optional point-start point-end visual-start visual-end)
  "Validate the current buffer according to STRING.
If STRING contains an occurrence of POINT-START immediately
followed by POINT-END, that position is compared against point.
If STRING contains an occurrence of VISUAL-START followed by
VISUAL-END, those positions are compared against the Visual selection.
POINT-START and POINT-END default to [ and ].
VISUAL-START and VISUAL-END default to < and >."
  (let ((actual-buffer (current-buffer))
        (marker-buffer (evil-test-marker-buffer-from-string
                        string
                        point-start point-end
                        visual-start visual-end))
        before-point after-point string selection)
    (unwind-protect
        (with-current-buffer marker-buffer
          (setq string (buffer-string))
          (when evil-test-point
            (setq before-point (buffer-substring (point-min) evil-test-point)
                  after-point (buffer-substring evil-test-point (point-max))))
          (when (and evil-test-visual-start evil-test-visual-end)
            (setq selection (buffer-substring
                             evil-test-visual-start evil-test-visual-end)))
          (with-current-buffer actual-buffer
            (if (or before-point after-point)
                (evil-test-text before-point after-point)
              ;; if the cursor isn't specified, just test the whole buffer
              (save-excursion
                (goto-char (point-min))
                (evil-test-text nil string #'bobp #'eobp)))
            (when selection
              (evil-test-selection selection))))
      (kill-buffer marker-buffer))))

(defun evil-test-buffer-from-string
  (string &optional state point-start point-end
          visual visual-start visual-end)
  "Create a new buffer according to STRING.
If STRING contains an occurrence of POINT-START immediately
followed by POINT-END, then point is moved to that position.
If STRING contains an occurrence of VISUAL-START followed by
VISUAL-END, then a Visual selection is created with those boundaries.
POINT-START and POINT-END default to [ and ].
VISUAL-START and VISUAL-END default to < and >.
STATE is the initial state; it defaults to `normal'.
VISUAL is the Visual selection: it defaults to `char'."
  (let ((type (evil-visual-type (or visual 'char)))
        (buffer (evil-test-marker-buffer-from-string
                 string point-start point-end
                 visual-start visual-end)))
    (with-current-buffer buffer
      (prog1 buffer
        (evil-change-state state)
        ;; let the buffer change its major mode without disabling Evil
        (add-hook 'after-change-major-mode-hook #'evil-initialize)
        (when (and (markerp evil-test-visual-start)
                   (markerp evil-test-visual-end))
          (evil-visual-select
           evil-test-visual-start evil-test-visual-end type)
          (when evil-test-point
            (goto-char evil-test-point)
            (evil-visual-refresh)
            (unless (and (= evil-visual-beginning
                            evil-test-visual-start)
                         (= evil-visual-end
                            evil-test-visual-end))
              (evil-visual-select
               evil-test-visual-start evil-test-visual-end type -1)
              (goto-char evil-test-point)
              (evil-visual-refresh))))
        (when (markerp evil-test-point)
          (goto-char evil-test-point))))))

(defun evil-test-marker-buffer-from-string
  (string &optional point-start point-end visual-start visual-end)
  "Create a new marker buffer according to STRING.
If STRING contains an occurrence of POINT-START immediately
followed by POINT-END, that position is stored in the
buffer-local variable `evil-test-point'. Similarly,
if STRING contains an occurrence of VISUAL-START followed by
VISUAL-END, those positions are stored in the variables
`evil-test-visual-beginning' and `evil-test-visual-end'.
POINT-START and POINT-END default to [ and ].
VISUAL-START and VISUAL-END default to < and >."
  (let ((string (or string ""))
        (point-start (regexp-quote
                      (if (characterp point-start)
                          (string point-start)
                        (or point-start "["))))
        (point-end (regexp-quote
                    (if (characterp point-end)
                        (string point-end)
                      (or point-end "]"))))
        (visual-start (regexp-quote
                       (if (characterp visual-start)
                           (string visual-start)
                         (or visual-start "<"))))
        (visual-end (regexp-quote
                     (if (characterp visual-end)
                         (string visual-end)
                       (or visual-end ">")))))
    (with-current-buffer (generate-new-buffer " *test*")
      (prog1 (current-buffer)
        (save-excursion
          (insert string))
        (save-excursion
          (when (> (length point-start) 0)
            (if (> (length point-end) 0)
                (when (re-search-forward
                       (format "\\(%s\\)[^%s]?\\(%s\\)"
                               point-start point-end point-end) nil t)
                  (goto-char (match-beginning 0))
                  (delete-region (match-beginning 2) (match-end 2))
                  (delete-region (match-beginning 1) (match-end 1))
                  (setq evil-test-point
                        (move-marker (make-marker) (point))))
              (when (re-search-forward point-start nil t)
                (goto-char (match-beginning 0))
                (delete-region (match-beginning 0) (match-end 0))
                (setq evil-test-point
                      (move-marker (make-marker) (point)))))))
        (save-excursion
          (when (and (> (length visual-start) 0)
                     (> (length visual-end) 0))
            (when (re-search-forward visual-start nil t)
              (goto-char (match-beginning 0))
              (delete-region (match-beginning 0) (match-end 0))
              (setq evil-test-visual-start
                    (move-marker (make-marker) (point))))
            (when (re-search-forward visual-end nil t)
              (goto-char (match-beginning 0))
              (delete-region (match-beginning 0) (match-end 0))
              (setq evil-test-visual-end
                    (move-marker (make-marker) (point))))))))))

(defun evil-test-text
  (before after &optional before-predicate after-predicate)
  "Verify the text around point.
BEFORE is the expected text before point, and AFTER is
the text after point. BEFORE-PREDICATE is a predicate function
to execute at the beginning of the text, and AFTER-PREDICATE
is executed at the end."
  (when before
    (if (functionp before)
        (setq before-predicate before
              before nil)
      (should (string= (buffer-substring
                        (max (point-min) (- (point) (length before)))
                        (point))
                       before))))
  (when after
    (if (functionp after)
        (setq after-predicate after
              after nil)
      (should (string= (buffer-substring
                        (point)
                        (min (point-max) (+ (point) (length after))))
                       after))))
  (when before-predicate
    (ert-info ((format "Expect `%s' at the beginning" before-predicate))
      (save-excursion
        (backward-char (length before))
        (should (funcall before-predicate)))))
  (when after-predicate
    (ert-info ((format "Expect `%s' at the end" after-predicate))
      (save-excursion
        (forward-char (length after))
        (should (funcall after-predicate))))))

(defmacro evil-test-selection
  (string &optional end-string before-predicate after-predicate)
  "Verify that the Visual selection contains STRING."
  (declare (indent defun))
  `(progn
     (save-excursion
       (goto-char (or evil-visual-beginning (region-beginning)))
       (evil-test-text nil (or ,string ,end-string) ,before-predicate))
     (save-excursion
       (goto-char (or evil-visual-end (region-end)))
       (evil-test-text (or ,end-string ,string) nil nil ,after-predicate))))

(defmacro evil-test-region
  (string &optional end-string before-predicate after-predicate)
  "Verify that the region contains STRING."
  (declare (indent defun))
  `(progn
     (save-excursion
       (goto-char (region-beginning))
       (evil-test-text nil (or ,string ,end-string) ,before-predicate))
     (save-excursion
       (goto-char (region-end))
       (evil-test-text (or ,end-string ,string) nil nil ,after-predicate))))

(defmacro evil-test-overlay
  (overlay string &optional end-string before-predicate after-predicate)
  "Verify that OVERLAY contains STRING."
  (declare (indent defun))
  `(progn
     (save-excursion
       (goto-char (overlay-start ,overlay))
       (evil-test-text nil (or ,string ,end-string) ,before-predicate))
     (save-excursion
       (goto-char (overlay-end ,overlay))
       (evil-test-text (or ,end-string ,string) nil nil ,after-predicate))))

(defmacro evil-with-temp-file (file-var content &rest body)
  "Create a temp file with CONTENT and bind its name to FILE-VAR within BODY.
FILE-VAR must be a symbol which contains the name of the
temporary file within the macro body. CONTENT is either a string
to be used as the content of the temporary file or a form to be
executed with the temporary file's buffer as \(current-buffer),
see `with-temp-file'. BODY contains the forms to be executed
while the temporary file exists. The temporary file is deleted at
the end of the execution of BODY."
  (declare (indent 2)
           (debug (symbolp form body)))
  `(let ((,file-var (make-temp-name
                     (expand-file-name "evil-test" temporary-file-directory))))
     (with-temp-file ,file-var
       ,(if (stringp content)
            `(insert ,content)
          content))
     ,@body
     (delete-file ,file-var)))

;;; States

(defun evil-test-local-mode-enabled ()
  "Verify that `evil-local-mode' is enabled properly"
  (ert-info ("Set the mode variable to t")
    (should (eq evil-local-mode t)))
  (ert-info ("Refresh `emulation-mode-map-alist'")
    (should (memq 'evil-mode-map-alist emulation-mode-map-alists)))
  (ert-info ("Create a buffer-local value for `evil-mode-map-alist'")
    (should (assq 'evil-mode-map-alist (buffer-local-variables))))
  (ert-info ("Initialize buffer-local keymaps")
    (should (assq 'evil-normal-state-local-map (buffer-local-variables)))
    (should (keymapp evil-normal-state-local-map))
    (should (assq 'evil-emacs-state-local-map (buffer-local-variables)))
    (should (keymapp evil-emacs-state-local-map)))
  (ert-info ("Don't add buffer-local entries to the default value")
    (should-not (rassq evil-normal-state-local-map
                       (default-value 'evil-mode-map-alist)))
    (should-not (rassq evil-emacs-state-local-map
                       (default-value 'evil-mode-map-alist)))))

(defun evil-test-local-mode-disabled ()
  "Verify that `evil-local-mode' is disabled properly"
  (ert-info ("Set the mode variable to nil")
    (should-not evil-local-mode))
  (ert-info ("Disable all states")
    (evil-test-no-states)))

(defun evil-test-no-states ()
  "Verify that all states are disabled"
  (ert-info ("Set `evil-state' to nil")
    (should-not evil-state))
  (ert-info ("Disable all state keymaps")
    (dolist (state (mapcar #'car evil-state-properties) t)
      (should-not (evil-state-property state :mode t))
      (should-not (memq (evil-state-property state :keymap t)
                        (current-active-maps)))
      (should-not (evil-state-property state :local t))
      (should-not (memq (evil-state-property state :local-keymap t)
                        (current-active-maps)))
      (dolist (map (evil-state-auxiliary-keymaps state))
        (should-not (memq map (current-active-maps)))))))

(ert-deftest evil-test-toggle-local-mode ()
  "Toggle `evil-local-mode'"
  :tags '(evil state)
  (with-temp-buffer
    (ert-info ("Enable `evil-local-mode'")
      (evil-local-mode 1)
      (evil-test-local-mode-enabled))
    (ert-info ("Disable `evil-local-mode'")
      (evil-local-mode -1)
      (evil-test-local-mode-disabled))))

(defun evil-test-change-state (state)
  "Change state to STATE and check keymaps"
  (let (mode keymap local-mode local-keymap tag)
    (evil-change-state state)
    (setq mode (evil-state-property state :mode)
          keymap (evil-state-property state :keymap t)
          local-mode (evil-state-property state :local)
          local-keymap (evil-state-property state :local-keymap t)
          tag (evil-state-property state :tag t))
    (ert-info ("Update `evil-state'")
      (should (eq evil-state state)))
    (ert-info ("Ensure `evil-local-mode' is enabled")
      (evil-test-local-mode-enabled))
    (ert-info ("Enable state modes")
      (should (symbol-value mode))
      (should (symbol-value local-mode)))
    (ert-info ("Push state keymaps to the top")
      (evil-test-state-keymaps state))
    (ert-info ("Refresh mode line tag")
      (should (equal evil-mode-line-tag tag)))))

(defun evil-test-state-keymaps (state)
  "Verify that STATE's keymaps are pushed to the top"
  (let ((actual (evil-state-keymaps state))
        (expected `((evil-esc-mode . ,evil-esc-map)
                    (,(evil-state-property state :local)
                     . , (evil-state-property state :local-keymap t))
                    (,(evil-state-property state :mode)
                     . ,(evil-state-property state :keymap t)))))
    ;; additional keymaps inherited with :enable
    (cond
     ((eq state 'operator)
      (setq expected
            `((evil-esc-mode . ,evil-esc-map)
              (evil-operator-shortcut-mode
               . ,evil-operator-shortcut-map)
              (evil-operator-state-local-minor-mode
               . ,evil-operator-state-local-map)
              (evil-operator-state-minor-mode
               . ,evil-operator-state-map)
              (evil-motion-state-local-minor-mode
               . ,evil-motion-state-local-map)
              (evil-motion-state-minor-mode
               . ,evil-motion-state-map)
              (evil-normal-state-local-minor-mode
               . ,evil-normal-state-local-map)
              (evil-normal-state-minor-mode
               . ,evil-normal-state-map)))))
    (let ((actual (butlast actual (- (length actual)
                                     (length expected)))))
      (should (equal actual expected))
      (dolist (map actual)
        (setq map (cdr-safe map))
        (should (keymapp map))
        ;; Emacs state disables `evil-esc-map'
        (unless (and (eq state 'emacs) (eq map evil-esc-map))
          (should (memq map (current-active-maps))))))))

(ert-deftest evil-test-exit-normal-state ()
  "Enter Normal state and then disable all states"
  :tags '(evil state)
  (with-temp-buffer
    (evil-test-change-state 'normal)
    (evil-normal-state -1)
    (evil-test-no-states)))

(ert-deftest evil-test-change-states ()
  "Change between Normal state, Emacs state and Operator-Pending state"
  :tags '(evil state)
  (with-temp-buffer
    (evil-test-change-state 'normal)
    (evil-test-change-state 'emacs)
    (evil-test-change-state 'normal)
    (evil-test-change-state 'operator)
    (evil-test-change-state 'normal)
    (evil-test-change-state 'emacs)
    (evil-test-change-state 'replace)
    (evil-test-change-state 'normal)))

(ert-deftest evil-test-enter-normal-state-disabled ()
  "Enter Normal state even if `evil-local-mode' is disabled"
  :tags '(evil state)
  (with-temp-buffer
    (evil-local-mode -1)
    (evil-test-local-mode-disabled)
    (evil-test-change-state 'normal)))

(defun evil-test-suppress-keymap (state)
  "Verify that `self-insert-command' is suppressed in STATE"
  (evil-test-buffer
    ";; This buffer is for notes."
    (evil-test-change-state state)
    ;; TODO: this should be done better
    (ert-info ("Disable the state's own keymaps so that the
suppression keymap comes first")
      (setq evil-operator-state-minor-mode nil
            evil-operator-state-local-minor-mode nil))
    (should (eq (key-binding "Q") #'undefined))
    (ert-info ("Don't insert text")
      ;; may or may not signal an error, depending on batch mode
      (condition-case nil
          (execute-kbd-macro "QQQ")
        (error nil))
      (should (string= (buffer-substring 1 4) ";; ")))))

(ert-deftest evil-test-emacs-state-suppress-keymap ()
  "`self-insert-command' works in Emacs state"
  :tags '(evil state)
  (should-error (evil-test-suppress-keymap 'emacs)))

(ert-deftest evil-test-normal-state-suppress-keymap ()
  "No `self-insert-command' in Normal state"
  :tags '(evil state)
  (evil-test-suppress-keymap 'normal))

(ert-deftest evil-test-operator-state-suppress-keymap ()
  "Operator-Pending state should inherit suppression
of `self-insert-command' from Normal state"
  :tags '(evil state)
  (evil-test-suppress-keymap 'operator))

(ert-deftest evil-test-operator-state-shortcut-keymap ()
  "Enable shortcut keymap in Operator-Pending state"
  :tags '(evil state)
  (evil-test-buffer
    (ert-info ("Activate `evil-operator-shortcut-map' in \
Operator-Pending state")
      (evil-test-change-state 'operator)
      (should (rassq evil-operator-shortcut-map
                     (evil-state-keymaps 'operator)))
      (should (keymapp evil-operator-shortcut-map))
      (should evil-operator-shortcut-mode)
      (should (memq evil-operator-shortcut-map
                    (current-active-maps))))
    (ert-info ("Deactivate `evil-operator-shortcut-map' \
outside Operator-Pending state")
      (evil-test-change-state 'emacs)
      (should-not evil-operator-shortcut-mode)
      (should-not (memq evil-operator-shortcut-map
                        (current-active-maps))))
    (ert-info ("Reset `evil-operator-shortcut-map' \
when entering Operator-Pending state")
      (define-key evil-operator-shortcut-map "f" 'foo)
      (should (eq (lookup-key evil-operator-shortcut-map "f")
                  'foo))
      (evil-test-change-state 'operator)
      (should-not (eq (lookup-key evil-operator-shortcut-map "f")
                      'foo)))
    (ert-info ("Reset `evil-operator-shortcut-map' \
when exiting Operator-Pending state")
      (define-key evil-operator-shortcut-map "b" 'bar)
      (should (eq (lookup-key evil-operator-shortcut-map "b")
                  'bar))
      (evil-test-change-state 'emacs)
      (should-not (eq (lookup-key evil-operator-shortcut-map "b")
                      'bar)))))

(ert-deftest evil-test-auxiliary-maps ()
  "Test auxiliary keymaps"
  :tags '(evil state)
  (let ((map (make-sparse-keymap)) aux)
    (ert-info ("Create a new auxiliary keymap")
      (evil-define-key 'normal map "f" 'foo)
      (setq aux (evil-get-auxiliary-keymap map 'normal))
      (should (evil-auxiliary-keymap-p aux))
      (should (eq (lookup-key aux "f") 'foo)))
    (ert-info ("Add to auxiliary keymap")
      (evil-define-key 'normal map "b" 'bar)
      (should (eq (lookup-key aux "f") 'foo))
      (should (eq (lookup-key aux "b") 'bar)))))

;;; Type system

(ert-deftest evil-test-exclusive-type ()
  "Expand and contract the `line' type"
  :tags '(evil type)
  (evil-test-buffer
    ";; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
    (let* ((first-line 1)
           (second-line (progn
                          (forward-line)
                          (point)))
           (third-line (progn
                         (forward-line)
                         (point))))
      (ert-info ("Return the beginning and end unchanged \
if they are the same")
        (should (equal (evil-normalize 1 1 'exclusive)
                       (list 1 1 'exclusive))))
      (ert-info ("expand to `inclusive' if the end position \
is at the beginning of a line")
        (should (equal (evil-normalize (1+ first-line) second-line 'exclusive)
                       (list (1+ first-line) (1- second-line) 'inclusive
                             :expanded t))))
      (ert-info ("expand to `line' if both the beginning and end \
are at the beginning of a line")
        (should (equal (evil-normalize first-line second-line 'exclusive)
                       (list first-line second-line 'line
                             :expanded t))))
      (ert-info ("Measure as the strict difference between the end \
and the beginning")
        (should (string= (evil-describe 1 1 'exclusive)
                         "0 characters"))
        (should (string= (evil-describe 1 2 'exclusive)
                         "1 character"))
        (should (string= (evil-describe 5 2 'exclusive)
                         "3 characters"))))))

(ert-deftest evil-test-inclusive-type ()
  "Expand and contract the `inclusive' type"
  :tags '(evil type)
  (evil-test-buffer
    ";; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
    (ert-info ("Include the ending character")
      (should (equal (evil-expand 1 1 'inclusive)
                     '(1 2 inclusive :expanded t))))
    (ert-info ("Don't mind if positions are in wrong order")
      (should (equal (evil-expand 5 2 'inclusive)
                     '(2 6 inclusive :expanded t))))
    (ert-info ("Exclude the ending character when contracting")
      (should (equal (evil-contract 1 2 'inclusive)
                     '(1 1 inclusive :expanded nil))))
    (ert-info ("Don't mind positions' order when contracting")
      (should (equal (evil-contract 6 2 'inclusive)
                     '(2 5 inclusive :expanded nil))))
    (ert-info ("Measure as one more than the difference")
      (should (string= (evil-describe 1 1 'inclusive)
                       "1 character"))
      (should (string= (evil-describe 5 2 'inclusive)
                       "4 characters")))))

(ert-deftest evil-test-line-type ()
  "Expand the `line' type"
  :tags '(evil type)
  (evil-test-buffer
    ";; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
    (let* ((first-line 1)
           (second-line (progn
                          (forward-line)
                          (point)))
           (third-line (progn
                         (forward-line)
                         (point))))
      (ert-info ("Expand to the whole first line")
        (should (equal (evil-expand first-line first-line 'line)
                       (list first-line second-line 'line :expanded t)))
        (should (string= (evil-describe first-line first-line 'line)
                         "1 line")))
      (ert-info ("Expand to the two first lines")
        (should (equal (evil-expand first-line second-line 'line)
                       (list first-line third-line 'line :expanded t)))
        (should (string= (evil-describe first-line second-line 'line)
                         "2 lines"))))))

(ert-deftest evil-test-block-type ()
  "Expand and contract the `block' type"
  :tags '(evil type)
  (evil-test-buffer
    ";; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
    (let* ((first-line 1)
           (second-line (progn
                          (forward-line)
                          (point)))
           (third-line (progn
                         (forward-line)
                         (point))))
      (ert-info ("Expand to a 1x1 block")
        (should (equal (evil-expand 1 1 'block)
                       (list 1 2 'block :expanded t)))
        (should (string= (evil-describe 1 1 'block)
                         "1 row and 1 column")))
      (ert-info ("Expand to a 2x1 block")
        (should (equal (evil-expand first-line second-line 'block)
                       (list first-line (1+ second-line) 'block :expanded t)))
        (should (string= (evil-describe first-line second-line 'block)
                         "2 rows and 1 column")))
      (ert-info ("Expand to a 3x2 block")
        (should (equal (evil-expand first-line (1+ third-line) 'block)
                       (list first-line (1+ (1+ third-line))
                             'block :expanded t)))
        (should (string= (evil-describe first-line (1+ third-line) 'block)
                         "3 rows and 2 columns")))
      (ert-info ("Contract to a 0x0 rectangle")
        (should (equal (evil-contract 1 2 'block)
                       (list 1 1 'block :expanded nil))))
      (ert-info ("Contract to a 2x0 rectangle")
        (should (equal (evil-contract first-line (1+ second-line) 'block)
                       (list first-line second-line 'block :expanded nil))))
      (ert-info ("Contract to a 3x1 rectangle")
        (should (equal (evil-contract first-line (1+ (1+ third-line)) 'block)
                       (list first-line (1+ third-line)
                             'block :expanded nil)))))))

(ert-deftest evil-test-type-transform ()
  "Test `evil-transform'"
  :tags '(evil type)
  (evil-test-buffer
    ";; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
    (ert-info ("Return positions unchanged when passed nil \
for TYPE or TRANSFORM")
      (should (equal (evil-transform nil 1 2 'block)
                     '(1 2 block)))
      (should (equal (evil-transform 'expand 1 2 nil)
                     '(1 2)))
      (should (equal (evil-transform nil 1 2 nil)
                     '(1 2))))
    (ert-info ("Accept markers, but return positions")
      (should (equal (evil-transform 'expand
                                     (move-marker (make-marker) 1) 1
                                     'inclusive)
                     '(1 2 inclusive :expanded t)))
      (should (equal (evil-transform nil (move-marker (make-marker) 1) 2
                                     nil)
                     '(1 2))))))

(ert-deftest evil-test-type-modifiers ()
  "Test type modifiers like \"dv}\""
  :tags '(evil type)
  (ert-info ("Change `inclusive' motions to `exclusive'")
    (evil-test-buffer
      "[A]bove some line"
      ("dve")
      "[e] some line"))
  (ert-info ("Change `exclusive' motions to `inclusive'")
    (evil-test-buffer
      "Above [s]ome line

Below some empty line"
      ("dv}")
      "Above[ ]
Below some empty line"))
  (ert-info ("Change type to `line'")
    (evil-test-buffer
      "Above [s]ome line

Below some empty line"
      ("dV}")
      "[B]elow some empty line")))

;;; Insertion

(ert-deftest evil-test-insert ()
  "Test `evil-insert'"
  :tags '(evil insert)
  (evil-test-buffer
    ";; [T]his buffer is for notes you don't want to save"
    ("ievil rulz " [escape])
    ";; evil rulz[ ]This buffer is for notes you don't want to save"))

(ert-deftest evil-test-append ()
  "Test `evil-append'"
  :tags '(evil insert)
  (evil-test-buffer
    ";; [T]his buffer is for notes you don't want to save"
    ("aevil rulz " [escape])
    ";; Tevil rulz[ ]his buffer is for notes you don't want to save"))

(ert-deftest evil-test-open-above ()
  "Test `evil-open-above'"
  :tags '(evil insert)
  (evil-test-buffer
    ";; This buffer is for notes you don't want to save,
\[;]; and for Lisp evaluation."
    ("Oabc\ndef" [escape])
    ";; This buffer is for notes you don't want to save,
abc
de[f]
;; and for Lisp evaluation."))

(ert-deftest evil-test-open-below ()
  "Test `evil-open-below'"
  :tags '(evil insert)
  (evil-test-buffer
    "[;]; This buffer is for notes you don't want to save,
;; and for Lisp evaluation."
    ("oabc\ndef" [escape])
    ";; This buffer is for notes you don't want to save,
abc
de[f]
;; and for Lisp evaluation."))

(ert-deftest evil-test-insert-line ()
  "Test `evil-insert-line'"
  :tags '(evil insert)
  (evil-test-buffer
    ";; [T]his buffer is for notes you don't want to save"
    ("Ievil rulz " [escape])
    "evil rulz[ ];; This buffer is for notes you don't want to save"))

(ert-deftest evil-test-append-line ()
  "Test `evil-append-line'"
  :tags '(evil insert)
  (evil-test-buffer
    ";; [T]his buffer is for notes you don't want to save"
    ("Aevil rulz " [escape])
    ";; This buffer is for notes you don't want to saveevil rulz[ ]"))

(ert-deftest evil-test-insert-digraph ()
  "Test `evil-insert-digraph'"
  :tags '(evil insert)
  (ert-info ("Predefined digraph")
    (evil-test-buffer
      ("i\C-kae")
      "æ[]"))
  (ert-info ("Custom digraph")
    (let ((evil-digraphs-table-user '(((?a ?o) . ?å))))
      (evil-test-buffer
        ("i\C-kao")
        "å[]"))))

;;; Repeat system

(ert-deftest evil-test-normalize-repeat-info ()
  "Test `evil-normalize-repeat-info'"
  :tags '(evil repeat)
  (ert-info ("Single array")
    (should (equal (evil-normalize-repeat-info
                    '("abc"))
                   '([?a ?b ?c])))
    (should (equal (evil-normalize-repeat-info
                    '("\M-f"))
                   (list (kbd "M-f")))))
  (ert-info ("Single symbol")
    (should (equal (evil-normalize-repeat-info
                    '(SYM))
                   '(SYM))))
  (ert-info ("Arrays only")
    (should (equal (evil-normalize-repeat-info
                    '("abc" [XX YY] "def"))
                   '([?a ?b ?c XX YY ?d ?e ?f]))))
  (ert-info ("Several symbols")
    (should (equal (evil-normalize-repeat-info
                    '(BEG MID END))
                   '(BEG MID END))))
  (ert-info ("Arrays with symbol at the beginning")
    (should (equal (evil-normalize-repeat-info
                    '(BEG "abc" [XX YY] "def"))
                   '(BEG [?a ?b ?c XX YY ?d ?e ?f]))))
  (ert-info ("Arrays with symbol at the end")
    (should (equal (evil-normalize-repeat-info
                    '("abc" [XX YY] "def" END))
                   '([?a ?b ?c XX YY ?d ?e ?f] END))))
  (ert-info ("Arrays with symbol in the middle")
    (should (equal (evil-normalize-repeat-info
                    '("abc" [XX YY] MID "def" ))
                   '([?a ?b ?c XX YY] MID [?d ?e ?f]))))
  (ert-info ("Concatenate arrays with several symbols")
    (should (equal (evil-normalize-repeat-info
                    '(BEG "abc" [XX YY] MID "def" END))
                   '(BEG [?a ?b ?c XX YY] MID [?d ?e ?f] END)))))

(defun evil-test-repeat-info (keys &optional recorded)
  "Execute a sequence of keys and verify that `evil-repeat-ring'
records them correctly. KEYS is the sequence of keys to execute.
RECORDED is the expected sequence of recorded events.
If nil, KEYS is used."
  (execute-kbd-macro keys)
  (should (equal (evil-normalize-repeat-info (ring-ref evil-repeat-ring 0))
                 (list (vconcat (or recorded keys))))))

(ert-deftest evil-test-normal-repeat-info-simple-command ()
  "Save key-sequence after simple editing command in Normal state"
  :tags '(evil repeat)
  (evil-test-buffer
    "[T]his is a test buffer"
    (ert-info ("Call simple command without count")
      (evil-test-repeat-info "x"))
    (ert-info ("Call simple command with count 3")
      (evil-test-repeat-info "3x"))))

(ert-deftest evil-test-normal-repeat-info-char-command ()
  "Save key-sequence after editing command with character in Normal state"
  :tags '(evil repeat)
  (evil-test-buffer
    "[T]his is a test buffer"
    (ert-info ("Call command with character argument without count")
      (evil-test-repeat-info "r5"))
    (ert-info ("Call command with character argument with count 12")
      (evil-test-repeat-info "12rX"))))

(ert-deftest evil-test-insert-repeat-info ()
  "Save key-sequence after Insert state"
  :tags '(evil repeat)
  (evil-test-buffer
    (ert-info ("Insert text without count")
      (evil-test-repeat-info (vconcat "iABC" [escape])))
    (ert-info ("Insert text with count 42")
      (evil-test-repeat-info (vconcat "42iABC" [escape])))))

(ert-deftest evil-test-repeat ()
  "Repeat several editing commands"
  :tags '(evil repeat)
  (ert-info ("Repeat replace")
    (evil-test-buffer
      "[;]; This buffer is for notes you don't want to save"
      ("rX")
      "[X]; This buffer is for notes you don't want to save"
      ([right right] ".")
      "X;[X]This buffer is for notes you don't want to save"))
  (ert-info ("Repeat replace with count")
    (evil-test-buffer
      "[;]; This buffer is for notes you don't want to save"
      ("2rX")
      "X[X] This buffer is for notes you don't want to save"
      ([right right] ".")
      "XX X[X]is buffer is for notes you don't want to save"))
  (ert-info ("Repeat replace without count with a new count")
    (evil-test-buffer
      "[;]; This buffer is for notes you don't want to save"
      ("rX")
      "[X]; This buffer is for notes you don't want to save"
      ([right right] "13.")
      "X;XXXXXXXXXXXX[X]is for notes you don't want to save"))
  (ert-info ("Repeat replace with count replacing original count")
    (evil-test-buffer
      "[;]; This buffer is for notes you don't want to save"
      ("10rX")
      "XXXXXXXXX[X]ffer is for notes you don't want to save"
      ([right right] "20.")
      "XXXXXXXXXXfXXXXXXXXXXXXXXXXXXX[X] don't want to save"))
  (ert-info ("Repeat movement in Insert state")
    (evil-test-buffer
      ";; [T]his buffer is for notes you don't want to save"
      ("i(\M-f)" [escape])
      ";; (This[)] buffer is for notes you don't want to save"
      ("w.")
      ";; (This) (buffer[)] is for notes you don't want to save")))

(ert-deftest evil-test-cmd-replace-char ()
  "Calling `evil-replace-char' should replace characters"
  :tags '(evil repeat)
  (evil-test-buffer
    "[;]; This buffer is for notes you don't want to save"
    ("r5")
    "[5]; This buffer is for notes you don't want to save"
    ("3rX")
    "XX[X]This buffer is for notes you don't want to save"))

(ert-deftest evil-test-insert-with-count ()
  "Test `evil-insert' with repeat count"
  :tags '(evil repeat)
  (evil-test-buffer
    ";; [T]his buffer is for notes"
    ("2ievil rulz " [escape])
    ";; evil rulz evil rulz[ ]This buffer is for notes"))

(ert-deftest evil-test-repeat-insert ()
  "Test repeating of `evil-insert'"
  :tags '(evil repeat)
  (ert-info ("Repeat insert")
    (evil-test-buffer
      "[;]; This buffer is for notes"
      ("iABC" [escape])
      "AB[C];; This buffer is for notes"
      ("..")
      "ABABAB[C]CC;; This buffer is for notes"))
  (ert-info ("Repeat insert with count")
    (evil-test-buffer
      "[;]; This buffer is for notes"
      ("2iABC" [escape])
      "ABCAB[C];; This buffer is for notes"
      ("..")
      "ABCABABCABABCAB[C]CC;; This buffer is for notes"))
  (ert-info ("Repeat insert with repeat count")
    (evil-test-buffer
      "[;]; This buffer is for notes"
      ("iABC" [escape])
      "AB[C];; This buffer is for notes"
      ("11.")
      "ABABCABCABCABCABCABCABCABCABCABCAB[C]C;; This buffer is for notes"))
  (ert-info ("Repeat insert with count with repeat with count")
    (evil-test-buffer
      "[;]; This buffer is for notes"
      ("10iABC" [escape])
      "ABCABCABCABCABCABCABCABCABCAB[C];; This buffer is for notes"
      ("11.")
      "ABCABCABCABCABCABCABCABCABCABABCABCABCABCABCABCABCABCABCABCAB[C]C;; \
This buffer is for notes")))

(ert-deftest evil-test-insert-vcount ()
  "Test `evil-insert' with vertical repeating"
  :tags '(evil repeat)
  (evil-test-buffer
    ";; [T]his buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

;; Below the empty line."
    (define-key evil-normal-state-local-map "i"
      #'(lambda (count)
          (interactive "p")
          (evil-insert count 5)))
    ("2iABC" [escape])
    "\
;; ABCAB[C]This buffer is for notes you don't want to save.
;; ABCABCIf you want to create a file, visit that file with C-x C-f,
;; ABCABCthen enter the text in that file's own buffer.
   ABCABC
;; ABCABCBelow the empty line."))

(ert-deftest evil-test-append-with-count ()
  "Test `evil-append' with repeat count"
  :tags '(evil repeat)
  (evil-test-buffer
    ";; [T]his buffer is for notes"
    ("2aevil rulz " [escape])
    ";; Tevil rulz evil rulz[ ]his buffer is for notes"))

(ert-deftest evil-test-repeat-append ()
  "Test repeating of `evil-append'"
  :tags '(evil repeat)
  (ert-info ("Repeat insert")
    (evil-test-buffer
      "[;]; This buffer is for notes"
      ("aABC" [escape])
      ";AB[C]; This buffer is for notes"
      ("..")
      ";ABCABCAB[C]; This buffer is for notes"))
  (ert-info ("Repeat insert with count")
    (evil-test-buffer
      "[;]; This buffer is for notes"
      ("2aABC" [escape])
      ";ABCAB[C]; This buffer is for notes"
      ("..")
      ";ABCABCABCABCABCAB[C]; This buffer is for notes"))
  (ert-info ("Repeat insert with repeat count")
    (evil-test-buffer
      "[;]; This buffer is for notes"
      ("aABC" [escape])
      ";AB[C]; This buffer is for notes"
      ("11.")
      ";ABCABCABCABCABCABCABCABCABCABCABCAB[C]; This buffer is for notes"))
  (ert-info ("Repeat insert with count with repeat with count")
    (evil-test-buffer
      "[;]; This buffer is for notes"
      ("10aABC" [escape])
      ";ABCABCABCABCABCABCABCABCABCAB[C]; This buffer is for notes"
      ("11.")
      ";ABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCAB[C]; \
This buffer is for notes")))

(ert-deftest evil-test-append-vcount ()
  "Test `evil-append' with vertical repeating"
  :tags '(evil repeat)
  (evil-test-buffer
    ";; [T]his buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

;; Below the empty line."
    (define-key evil-normal-state-local-map "a"
      #'(lambda (count)
          (interactive "p")
          (evil-append count 5)))
    ("2aABC" [escape])
    "\
;; TABCAB[C]his buffer is for notes you don't want to save.
;; IABCABCf you want to create a file, visit that file with C-x C-f,
;; tABCABChen enter the text in that file's own buffer.
    ABCABC
;; BABCABCelow the empty line."))

(ert-deftest evil-test-open-above-with-count ()
  "Test `evil-open-above' with repeat count"
  :tags '(evil repeat)
  (evil-test-buffer
    ";; This buffer is for notes you don't want to save,
\[;]; and for Lisp evaluation."
    ("2Oevil\nrulz" [escape])
    ";; This buffer is for notes you don't want to save,
evil\nrulz\nevil\nrul[z]
;; and for Lisp evaluation."))

(ert-deftest evil-test-repeat-open-above ()
  "Test repeating of `evil-open-above'"
  :tags '(evil repeat)
  (ert-info ("Repeat insert")
    (evil-test-buffer
      "[;]; This buffer is for notes you don't want to save."
      ("Oevil\nrulz" [escape])
      "evil\nrul[z]
;; This buffer is for notes you don't want to save."
      ("..")
      "evil\nevil\nevil\nrul[z]\nrulz\nrulz
;; This buffer is for notes you don't want to save."))
  (ert-info ("Repeat insert with count")
    (evil-test-buffer
      ";; This buffer is for notes you don't want to save."
      ("2Oevil\nrulz" [escape])
      "evil\nrulz\nevil\nrul[z]
;; This buffer is for notes you don't want to save."
      ("..")
      "evil\nrulz\nevil\nevil\nrulz\nevil\nevil\nrulz\nevil\nrul[z]\nrulz\nrulz
;; This buffer is for notes you don't want to save."))
  (ert-info ("Repeat insert with repeat count")
    (evil-test-buffer
      ";; This buffer is for notes you don't want to save."
      ("Oevil\nrulz" [escape])
      "evil\nrul[z]\n;; This buffer is for notes you don't want to save."
      ("2.")
      "evil\nevil\nrulz\nevil\nrul[z]\nrulz
;; This buffer is for notes you don't want to save."))
  (ert-info ("Repeat insert with count with repeat with count")
    (evil-test-buffer
      ";; This buffer is for notes you don't want to save."
      ("2Oevil\nrulz" [escape])
      "evil\nrulz\nevil\nrul[z]
;; This buffer is for notes you don't want to save."
      ("3.")
      "evil\nrulz\nevil\nevil\nrulz\nevil\nrulz\nevil\nrul[z]\nrulz
;; This buffer is for notes you don't want to save.")))

(ert-deftest evil-test-open-below-with-count ()
  "Test insertion of `evil-open-below' with repeat count"
  :tags '(evil repeat)
  (evil-test-buffer
    "[;]; This buffer is for notes you don't want to save,
;; and for Lisp evaluation."
    ("2oevil\nrulz" [escape])
    ";; This buffer is for notes you don't want to save,
evil\nrulz\nevil\nrul[z]
;; and for Lisp evaluation."))

(ert-deftest evil-test-repeat-open-below ()
  "Test repeating `evil-open-below'"
  :tags '(evil repeat)
  (ert-info ("Repeat insert")
    (evil-test-buffer
      "[;]; This buffer is for notes you don't want to save,
;; and for Lisp evaluation."
      ("oevil\nrulz" [escape])
      ";; This buffer is for notes you don't want to save,
evil\nrul[z]\n;; and for Lisp evaluation."
      ("..")
      ";; This buffer is for notes you don't want to save,
evil\nrulz\nevil\nrulz\nevil\nrul[z]
;; and for Lisp evaluation."))
  (ert-info ("Repeat insert with count")
    (evil-test-buffer
      "[;]; This buffer is for notes you don't want to save,
;; and for Lisp evaluation."
      ("2oevil\nrulz" [escape])
      ";; This buffer is for notes you don't want to save,
evil\nrulz\nevil\nrul[z]
;; and for Lisp evaluation."
      ("..")
      ";; This buffer is for notes you don't want to save,
evil\nrulz\nevil\nrulz\nevil\nrulz\nevil\nrulz\nevil\nrulz\nevil\nrul[z]
;; and for Lisp evaluation."))
  (ert-info ("Repeat insert with repeat count")
    (evil-test-buffer
      "[;]; This buffer is for notes you don't want to save,
;; and for Lisp evaluation."
      ("oevil\nrulz" [escape])
      ";; This buffer is for notes you don't want to save,
evil\nrul[z]\n;; and for Lisp evaluation."
      ("2.")
      ";; This buffer is for notes you don't want to save,
evil\nrulz\nevil\nrulz\nevil\nrul[z]
;; and for Lisp evaluation."))
  (ert-info ("Repeat insert with count with repeat with count")
    (evil-test-buffer
      "[;]; This buffer is for notes you don't want to save,
;; and for Lisp evaluation."
      ("2oevil\nrulz" [escape])
      ";; This buffer is for notes you don't want to save,
evil\nrulz\nevil\nrul[z]
;; and for Lisp evaluation."
      ("3.")
      ";; This buffer is for notes you don't want to save,
evil\nrulz\nevil\nrulz\nevil\nrulz\nevil\nrulz\nevil\nrul[z]
;; and for Lisp evaluation.")))

(ert-deftest evil-test-insert-line-with-count ()
  "Test `evil-insert-line' with repeat count"
  :tags '(evil repeat)
  (evil-test-buffer
    ";; [T]his buffer is for notes"
    ("2Ievil rulz " [escape])
    "evil rulz evil rulz[ ];; This buffer is for notes"))

(ert-deftest evil-test-repeat-insert-line ()
  "Test repeating of `evil-insert-line'"
  :tags '(evil repeat)
  (ert-info ("Repeat insert")
    (evil-test-buffer
      ";; This buffer is for note[s]"
      ("IABC" [escape])
      "AB[C];; This buffer is for notes"
      ("..")
      "AB[C]ABCABC;; This buffer is for notes"))
  (ert-info ("Repeat insert with count")
    (evil-test-buffer
      ";; This buffer is for note[s]"
      ("2IABC" [escape])
      "ABCAB[C];; This buffer is for notes"
      ("..")
      "ABCAB[C]ABCABCABCABC;; This buffer is for notes"))
  (ert-info ("Repeat insert with repeat count")
    (evil-test-buffer
      ";; This buffer is for note[s]"
      ("IABC" [escape])
      "AB[C];; This buffer is for notes"
      ("11.")
      "ABCABCABCABCABCABCABCABCABCABCAB[C]ABC;; This buffer is for notes"))
  (ert-info ("Repeat insert with count with repeat with count")
    (evil-test-buffer
      ";; This buffer is for note[s]"
      ("10IABC" [escape])
      "ABCABCABCABCABCABCABCABCABCAB[C];; This buffer is for notes"
      ("11.")
      "ABCABCABCABCABCABCABCABCABCABCAB[C]ABCABCABCABCABCABCABCABCABCABC;; This buffer is for notes")))

(ert-deftest evil-test-insert-line-vcount ()
  "Test `evil-insert-line' with vertical repeating"
  :tags '(evil repeat)
  (evil-test-buffer
    "int[ ]main(int argc, char** argv)
{
  printf(\"Hello world\\n\");
  return EXIT_SUCCESS;
}"
    (define-key evil-normal-state-local-map "I"
      #'(lambda (count)
          (interactive "p")
          (evil-insert-line count 4)))
    ("2IABC" [escape])
    "ABCABCint main(int argc, char** argv)
ABCABC{
  ABCABCprintf(\"Hello world\\n\");
  ABCABCreturn EXIT_SUCCESS;
}"))

(ert-deftest evil-test-append-line-with-count ()
  "Test `evil-append-line' with repeat count"
  :tags '(evil repeat)
  (evil-test-buffer
    ";; [T]his buffer is for notes."
    ("2Aevil rulz " [escape])
    ";; This buffer is for notes.evil rulz evil rulz[ ]"))

(ert-deftest evil-test-repeat-append-line ()
  "Test repeating of `evil-append-line'"
  :tags '(evil repeat)
  (ert-info ("Repeat insert")
    (evil-test-buffer
      ";; [T]his buffer is for notes."
      ("AABC" [escape])
      ";; This buffer is for notes.AB[C]"
      ("..")
      ";; This buffer is for notes.ABCABCAB[C]"))
  (ert-info ("Repeat insert with count")
    (evil-test-buffer
      ";; [T]his buffer is for notes."
      ("2AABC" [escape])
      ";; This buffer is for notes.ABCAB[C]"
      ("..")
      ";; This buffer is for notes.ABCABCABCABCABCAB[C]"))
  (ert-info ("Repeat insert with repeat count")
    (evil-test-buffer
      ";; [T]his buffer is for notes."
      ("AABC" [escape])
      ";; This buffer is for notes.ABC"
      ("11.")
      ";; This buffer is for notes.ABCABCABCABCABCABCABCABCABCABCABCAB[C]"))
  (ert-info ("Repeat insert with count with repeat with count")
    (evil-test-buffer
      ";; [T]his buffer is for notes."
      ("10AABC" [escape])
      ";; This buffer is for notes.ABCABCABCABCABCABCABCABCABCAB[C]"
      ("11.")
      ";; This buffer is for notes.ABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCAB[C]")))

(ert-deftest evil-test-append-line-vcount ()
  "Test `evil-append-line' with vertical repeating"
  :tags '(evil repeat)
  (evil-test-buffer
    "int[ ]main(int argc, char** argv)
{
  printf(\"Hello world\\n\");
  return EXIT_SUCCESS;
}"
    (define-key evil-normal-state-local-map "A"
      #'(lambda (count)
          (interactive "p")
          (evil-append-line count 4)))
    ("2AABC" [escape])
    "int main(int argc, char** argv)ABCAB[C]
{ABCABC
  printf(\"Hello world\\n\");ABCABC
  return EXIT_SUCCESS;ABCABC
}"))

(ert-deftest evil-test-repeat-by-change ()
  "Test repeating by tracking changes for completion commands"
  :tags '(evil repeat)
  (let ((line-move-visual nil)
        (change (evil-define-command nil ()
                  :repeat change
                  (interactive)
                  (delete-char 5)
                  (insert "BEGIN\n")
                  (save-excursion
                    (insert "\nEND\n")))))
    (evil-test-buffer
      ";; [T]his buffer is for notes."
      (define-key evil-insert-state-local-map (kbd "C-c C-p") change)
      ("iABC " (kbd "C-c C-p") "BODY" [escape])
      ";; ABC BEGIN
BOD[Y]
END
buffer is for notes."
      (".")
      ";; ABC BEGIN
BODABC BEGIN
BOD[Y]
END

buffer is for notes.")))

(ert-deftest evil-test-repeat-kill-buffer ()
  "Test safe-guard preventing buffers from being deleted
when repeating a command"
  :tags '(evil repeat)
  (ert-info ("Test killing works for direct calls \
to `evil-execute-repeat-info'")
    (evil-test-buffer
      "[;]; This buffer is for notes."
      (setq evil-repeat-ring (make-ring 10))
      (ring-insert evil-repeat-ring '((kill-buffer nil)))
      (evil-execute-repeat-info (ring-ref evil-repeat-ring 0))
      (should-not (looking-at ";; This"))))
  (ert-info ("Verify an error is raised when using \
the `evil-repeat' command")
    (evil-test-buffer
      "[;]; This buffer is for notes."
      (setq evil-repeat-ring (make-ring 10))
      (ring-insert evil-repeat-ring '((kill-buffer nil)))
      (evil-execute-repeat-info (ring-ref evil-repeat-ring 0))
      (should-error (call-interactively #'evil-repeat)))))

(ert-deftest evil-test-repeat-pop ()
  "Test `repeat-pop'."
  :tags '(evil repeat)
  (ert-info ("Test repeat-pop")
    (evil-test-buffer
      ";; [T]his buffer is for notes."
      (setq evil-repeat-ring (make-ring 10))
      ("iABC" [escape] "aXYZ" [escape])
      ";; ABCXY[Z]This buffer is for notes."
      (".")
      ";; ABCXYZXY[Z]This buffer is for notes."))
  (ert-info ("Test repeat-pop")
    (evil-test-buffer
      ";; [T]his buffer is for notes."
      (setq evil-repeat-ring (make-ring 10))
      ("iABC" [escape] "aXYZ" [escape])
      ";; ABCXY[Z]This buffer is for notes."
      ("." (kbd "C-."))
      ";; ABCXYAB[C]ZThis buffer is for notes."))
  (ert-info ("Test repeat-pop-next")
    (evil-test-buffer
      ";; [T]his buffer is for notes."
      (setq evil-repeat-ring (make-ring 10))
      ("iABC" [escape] "aXYZ" [escape])
      ";; ABCXY[Z]This buffer is for notes."
      ("." (kbd "C-.") (kbd "M-."))
      ";; ABCXYZXY[Z]This buffer is for notes."))
  (ert-info ("Test repeat-pop after non-change")
    (evil-test-buffer
      ";; [T]his buffer is for notes."
      (setq evil-repeat-ring (make-ring 10))
      ("iABC" [escape] "a" [escape] "aXYZ" [escape])
      ";; ABCXY[Z]This buffer is for notes."
      ("." (kbd "C-.") (kbd "C-."))
      ";; ABCXYAB[C]ZThis buffer is for notes.")))

(ert-deftest evil-test-ESC-repeat-normal-state ()
  "Test if ESC is not been recorded in normal state."
  :tags '(evil repeat)
  (ert-info ("Test normal ESC")
    (evil-test-buffer
      ";;[ ]This buffer is for notes."
      (setq evil-repeat-ring (make-ring 10))
      (should (= (ring-length evil-repeat-ring) 0))
      ("aABC" [escape])
      ";; AB[C]This buffer is for notes."
      (should (= (ring-length evil-repeat-ring) 1))
      (".")
      ";; ABCAB[C]This buffer is for notes."
      ([escape])
      (should (= (ring-length evil-repeat-ring) 1))
      (".")
      ";; ABCABCAB[C]This buffer is for notes.")))

(ert-deftest evil-test-abort-operator-repeat ()
  "Test if ESC in operator-state cancels recording of repeation."
  :tags '(evil repeat)
  (let ((inhibit-quit t))
    (ert-info ("Test ESC")
      (evil-test-buffer
        ";;[ ]This buffer is for notes."
        (setq evil-repeat-ring (make-ring 10))
        (should (= (ring-length evil-repeat-ring) 0))
        ("aABC" [escape])
        ";; AB[C]This buffer is for notes."
        (should (= (ring-length evil-repeat-ring) 1))
        (".")
        ";; ABCAB[C]This buffer is for notes."
        ("d" [escape])
        (should (= (ring-length evil-repeat-ring) 1))
        (".")
        ";; ABCABCAB[C]This buffer is for notes."))))

(ert-deftest evil-test-repeat-visual-char ()
  "Test repeat of character visual mode command."
  :tags '(evil repeat)
  (ert-info ("Test repeat on same line")
    (evil-test-buffer
      ";; [T]his buffer is for notes."
      ("v3lcABC" [escape])
      ";; AB[C] buffer is for notes."
      ("ww.")
      ";; ABC buffer AB[C]or notes."))
  (ert-info ("Test repeat on several lines")
    (evil-test-buffer
      ";; This [b]uffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.
"
      ("vj^eerX")
      ";; This XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXX[X] you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.
"
      ("2gg^3w.")
      ";; This XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXX you want XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXX[X]en enter the text in that file's own buffer.
")))

(ert-deftest evil-test-repeat-visual-line ()
  "Test repeat of linewise visual mode command."
  :tags '(evil repeat)
  (ert-info ("Test repeat on several lines")
    (evil-test-buffer
      ";; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter th[e] text in that file's own buffer.

;; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.
"
      ("VkcNew Text" [escape])
      ";; This buffer is for notes you don't want to save.
New Tex[t]

;; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.
"
      ("jj.")
      ";; This buffer is for notes you don't want to save.
New Text

New Tex[t]
;; then enter the text in that file's own buffer.
")))

(ert-deftest evil-test-repeat-visual-block ()
  "Test repeat of block visual mode command."
  :tags '(evil repeat)
  (ert-info ("Test repeat on several lines")
    (evil-test-buffer
      ";; This [b]uffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.
;; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.
"
      ((kbd "C-v") "3j2lrQ")
      ";; This [Q]QQfer is for notes you don't want to save.
;; If yoQQQant to create a file, visit that file with C-x C-f,
;; then QQQer the text in that file's own buffer.
;; This QQQfer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.
"
      ("2j3w.")
      ";; This QQQfer is for notes you don't want to save.
;; If yoQQQant to create a file, visit that file with C-x C-f,
;; then QQQer the text [Q]QQthat file's own buffer.
;; This QQQfer is for nQQQs you don't want to save.
;; If you want to creatQQQ file, visit that file with C-x C-f,
;; then enter the text QQQthat file's own buffer.
")))

;;; Operators

(ert-deftest evil-test-keypress-parser ()
  "Test `evil-keypress-parser'"
  :tags '(evil operator)
  (evil-test-buffer
    :state operator
    (ert-info ("Read from the keyboard unless INPUT is given")
      (evil-test-buffer
        :state operator
        (let ((unread-command-events '(?d)))
          (should (equal (evil-keypress-parser)
                         '(evil-delete nil)))
          (should (equal (evil-keypress-parser '(?d))
                         '(evil-delete nil))))))
    (ert-info ("Read remainder from the keyboard if INPUT is incomplete")
      (let ((unread-command-events '(?d)))
        (should (equal (evil-keypress-parser '(?2))
                       '(evil-delete 2)))))
    (ert-info ("Handle counts not starting with zero")
      (should (equal (evil-keypress-parser '(?2 ?d))
                     '(evil-delete 2)))
      (should (equal (evil-keypress-parser '(?2 ?0 ?d))
                     '(evil-delete 20)))
      (should (equal (evil-keypress-parser '(?2 ?0 ?2 ?d))
                     '(evil-delete 202)))
      (should (equal (evil-keypress-parser '(?4 ?0 ?4 ?g ??))
                     '(evil-rot13 404))))
    (ert-info ("Treat 0 as a motion")
      (should (equal
               (evil-keypress-parser '(?0))
               '(evil-digit-argument-or-evil-beginning-of-line nil))))))

(ert-deftest evil-test-invert-char ()
  "Test `evil-invert-char'"
  :tags '(evil operator)
  (evil-test-buffer
    ";; [T]his buffer is for notes."
    ("~")
    ";; t[h]is buffer is for notes.")
  (evil-test-buffer
    ";; <[T]his> buffer is for notes."
    ("~")
    ";; [t]HIS buffer is for notes.")
  (evil-test-buffer
    :visual block
    ";; <[T]his buffer is for notes,
;; and >for Lisp evaluation."
    ("~")
    ";; [t]HIS buffer is for notes,
;; AND for Lisp evaluation."))

(ert-deftest evil-test-rot13 ()
  "Test `evil-rot13'"
  :tags '(evil operator)
  (evil-test-buffer
    ";; [T]his buffer is for notes you don't want to save."
    ("g?" [M-right])
    ";; [G]uvf buffer is for notes you don't want to save."))

(ert-deftest evil-test-rot13-with-count ()
  "Test `evil-rot13' with count argument"
  :tags '(evil operator)
  (ert-info ("Count before operator")
    (evil-test-buffer
      ";; [T]his buffer is for notes you don't want to save."
      ("2g?" [M-right])
      ";; [G]uvf ohssre is for notes you don't want to save."))
  (ert-info ("Count before motion")
    (evil-test-buffer
      ";; [T]his buffer is for notes you don't want to save."
      ("g?2" [M-right])
      ";; [G]uvf ohssre is for notes you don't want to save."))
  (ert-info ("Count before operator and motion")
    (evil-test-buffer
      ";; [T]his buffer is for notes you don't want to save."
      ("3g?2" [M-right])
      ";; [G]uvf ohssre vf sbe abgrf lbh don't want to save."))
  (ert-info ("Count exceeding buffer boundaries")
    (evil-test-buffer
      ";; [T]his buffer is for notes you don't want to save."
      ("g?200" [right])
      ";; [G]uvf ohssre vf sbe abgrf lbh qba'g jnag gb fnir.")))

(ert-deftest evil-test-rot13-repeat ()
  "Test repeating of `evil-rot13'"
  :tags '(evil operator)
  (evil-test-buffer
    ";; [T]his buffer is for notes you don't want to save."
    ("g?" [M-right] [M-right])
    ";; Guvf[ ]buffer is for notes you don't want to save."
    (".")
    ";; Guvf[ ]ohssre is for notes you don't want to save."))

(ert-deftest evil-test-rot13-repeat-with-count ()
  "Test repeating of `evil-rot13' with new count"
  :tags '(evil operator)
  (ert-info ("Count before operator")
    (evil-test-buffer
      ";; [T]his buffer is for notes you don't want to save."
      ("2g?" [M-right])
      ";; [G]uvf ohssre is for notes you don't want to save."
      ("3.")
      ";; [T]his buffer vf for notes you don't want to save."))
  (ert-info ("Count before motion")
    (evil-test-buffer
      ";; [T]his buffer is for notes you don't want to save."
      ("g?2" [M-right])
      ";; [G]uvf ohssre is for notes you don't want to save."
      ("3.")
      ";; [T]his buffer vf for notes you don't want to save."))
  (ert-info ("Count before operator and motion")
    (evil-test-buffer
      ";; [T]his buffer is for notes you don't want to save."
      ("3g?2" [M-right])
      ";; [G]uvf ohssre vf sbe abgrf lbh don't want to save."
      ("4.")
      ";; [T]his buffer is for abgrf lbh don't want to save.")))

(ert-deftest evil-test-operator-delete ()
  "Test deleting text"
  :tags '(evil operator)
  (ert-info ("Delete characters")
    (evil-test-buffer
      ";; [T]his buffer is for notes."
      ("dl")
      ";; [h]is buffer is for notes."
      ("d1l")
      ";; [i]s buffer is for notes."
      ("1dl")
      ";; [s] buffer is for notes."
      ("1d1l")
      ";; [ ]buffer is for notes."
      ("d2l")
      ";; [u]ffer is for notes."
      ("2dl")
      ";; [f]er is for notes."
      ("d4l")
      ";; [i]s for notes."
      ("4dl")
      ";; [o]r notes."
      ("2d2l")
      ";; [o]tes."))
  (ert-info ("Delete current line")
    (evil-test-buffer
      ";; [T]his buffer is for notes you don't want to save,
;; and for Lisp evaluation."
      ("dd")
      "[;]; and for Lisp evaluation.")
    (evil-test-buffer
      ";; [T]his buffer is for notes you don't want to save,
;; and for Lisp evaluation."
      ("d1d")
      "[;]; and for Lisp evaluation.")
    (evil-test-buffer
      ";; [T]his buffer is for notes you don't want to save,
;; and for Lisp evaluation."
      ("1dd")
      "[;]; and for Lisp evaluation.")
    (evil-test-buffer
      ";; [T]his buffer is for notes you don't want to save,
;; and for Lisp evaluation."
      ("1d1d")
      "[;]; and for Lisp evaluation."))
  (ert-info ("Delete two lines")
    (evil-test-buffer
      ";; [T]his buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      ("d2d")
      "[;]; then enter the text in that file's own buffer.")
    (evil-test-buffer
      ";; [T]his buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      ("2dd")
      "[;]; then enter the text in that file's own buffer.")
    (evil-test-buffer
      ";; [T]his buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      ("dj")
      "[;]; then enter the text in that file's own buffer.")
    (evil-test-buffer
      ";; This buffer is for notes you don't want to save.
;; [I]f you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      ("dk")
      "[;]; then enter the text in that file's own buffer.")))

(evil-define-motion evil-test-square-motion (count)
  "Test motion for selecting a square."
  :type block
  (let ((column (current-column)))
    (forward-line (1- count))
    (move-to-column (+ column count -1))))

(ert-deftest evil-test-yank ()
  "Test `evil-yank'"
  :tags '(evil operator)
  (ert-info ("Yank characters")
    (evil-test-buffer
      ";; [T]his buffer is for notes you don't want to save."
      ("y2e")
      (should (string= (current-kill 0) "This buffer"))))
  (ert-info ("Yank lines")
    (evil-test-buffer
      ";; [T]his buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      ("yj")
      (should (string= (current-kill 0)
                       (buffer-substring (point-min)
                                         (1+ (line-end-position 2)))))
      (should (eq (car-safe (get-text-property 0 'yank-handler
                                               (current-kill 0)))
                  'evil-yank-line-handler)))
    (evil-test-buffer
      ";; This buffer is for notes you don't want to save.
\[;]; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      ("y5j")
      (should
       (string= (current-kill 0)
                (concat (buffer-substring (line-beginning-position 1)
                                          (point-max))
                        "\n")))
      (should (eq (car-safe (get-text-property 0 'yank-handler
                                               (current-kill 0)))
                  'evil-yank-line-handler))))
  (ert-info ("Yank rectangle")
    (evil-test-buffer
      ";; [T]his buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      (define-key evil-operator-state-local-map "s" 'evil-test-square-motion)
      ("y3s")
      (should (string= (current-kill 0) "Thi\nIf \nthe"))
      (should (eq (car-safe (get-text-property 0 'yank-handler
                                               (current-kill 0)))
                  'evil-yank-block-handler)))))

(ert-deftest evil-test-delete ()
  "Test `evil-delete'"
  :tags '(evil operator)
  (ert-info ("Delete characters")
    (evil-test-buffer
      ";; This buffer is for notes you don't want to save[.]"
      ("x")
      ";; This buffer is for notes you don't want to sav[e]"
      (goto-char 4)
      ";; [T]his buffer is for notes you don't want to save"
      ("d2e")
      ";; [ ]is for notes you don't want to save"
      (should (string= (current-kill 0) "This buffer"))
      ("P")
      ";; This buffe[r] is for notes you don't want to save"))
  (ert-info ("Delete lines")
    (evil-test-buffer
      ";; [T]his buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      ("2dd")
      "[;]; then enter the text in that file's own buffer."
      ("P")
      "[;]; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."))
  (ert-info ("Delete last line")
    (evil-test-buffer
      ";; This buffer is for notes you don't want to save.
;; [I]f you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      ("2dd")
      "[;]; This buffer is for notes you don't want to save."))
  (ert-info ("Delete rectangle")
    (evil-test-buffer
      "[;]; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      (define-key evil-operator-state-local-map "s" 'evil-test-square-motion)
      ("d3s")
      "[T]his buffer is for notes you don't want to save.
If you want to create a file, visit that file with C-x C-f,
then enter the text in that file's own buffer.")))

(ert-deftest evil-test-delete-line ()
  "Test `evil-delete-line'"
  :tags '(evil operator)
  (ert-info ("Delete to end of line")
    (evil-test-buffer
      ";; This buffer is for notes[ ]you don't want to save."
      ("D")
      ";; This buffer is for note[s]"))
  (ert-info ("Act linewise on character selection")
    (evil-test-buffer
      ";; This <buffe[r]> is for notes,
and for Lisp evaluation."
      ("D")
      "[a]nd for Lisp evaluation."))
  (ert-info ("Act on each line of block selection")
    (evil-test-buffer
      :visual block
      ";; This buffer is for <notes,
;; and for Lisp evaluatio[n]>."
      ("D")
      ";; This buffer is for[ ]
;; and for Lisp evalua")))

(ert-deftest evil-test-change ()
  "Test `evil-change'"
  :tags '(evil operator)
  (ert-info ("Change characters")
    (evil-test-buffer
      ";; [T]his buffer is for notes you don't want to save."
      ("c2eABC" [escape])
      ";; AB[C] is for notes you don't want to save."
      (should (string= (current-kill 0) "This buffer"))
      ("p")
      ";; ABCThis buffe[r] is for notes you don't want to save."))
  (ert-info ("Change lines")
    (evil-test-buffer
      ";; [T]his buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      ("2ccABCLINE\nDEFLINE" [escape])
      "ABCLINE
DEFLIN[E]
;; then enter the text in that file's own buffer."
      ("p")
      "ABCLINE
DEFLINE
\[;]; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."))
  (ert-info ("Change last line")
    (evil-test-buffer
      ";; This buffer is for notes you don't want to save.
;; [I]f you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      ("2ccABC" [escape])
      ";; This buffer is for notes you don't want to save.
AB[C]"))
  (ert-info ("Change rectangle")
    (evil-test-buffer
      "[;]; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      (define-key evil-operator-state-local-map "s" 'evil-test-square-motion)
      ("c3sABC" [escape])
      "AB[C]This buffer is for notes you don't want to save.
ABCIf you want to create a file, visit that file with C-x C-f,
ABCthen enter the text in that file's own buffer.")))

(ert-deftest evil-test-change-word ()
  "Test changing words"
  :tags '(evil operator)
  (ert-info ("Non-word")
    (evil-test-buffer
      "[;]; This buffer is for notes."
      ("cwABC" [escape])
      "AB[C] This buffer is for notes."))
  (ert-info ("Word")
    (evil-test-buffer
      ";; [T]his buffer is for notes."
      ("cwABC" [escape])
      ";; AB[C] buffer is for notes."))
  (ert-info ("Single character")
    (evil-test-buffer
      "[;] This buffer is for notes."
      ("cwABC" [escape])
      "AB[C] This buffer is for notes.")))

(ert-deftest evil-test-join ()
  "Test `evil-join'"
  :tags '(evil operator)
  (ert-info ("Simple")
    (evil-test-buffer
      ";; [T]his buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f."
      ("J")
      ";; This buffer is for notes you don't want to save.[ ]\
;; If you want to create a file, visit that file with C-x C-f."))
  (ert-info ("Visual")
    (evil-test-buffer
      :visual line
      "<;; [T]his buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f.>"
      ("J")
      ";; This buffer is for notes you don't want to save.[ ]\
;; If you want to create a file, visit that file with C-x C-f.")))

(ert-deftest evil-test-substitute ()
  "Test `evil-substitute'"
  :tags '(evil operator)
  (ert-info ("Simple")
    (evil-test-buffer
      ";; [T]his buffer is for notes."
      ("5sABC" [escape])
      ";; AB[C]buffer is for notes."))
  (ert-info ("On empty line")
    (evil-test-buffer
      "Above some line
\[]
Below some empty line"
      ("5sABC" [escape])
      "Above some line
AB[C]
Below some empty line")))

;;; Paste

(ert-deftest evil-test-paste-before ()
  "Test `evil-paste-before'"
  :tags '(evil paste)
  (ert-info ("Paste characters")
    (evil-test-buffer
      ";; [T]his buffer is for notes you don't want to save,
;; and for Lisp evaluation."
      ("y2ej0")
      ";; This buffer is for notes you don't want to save,
\[;]; and for Lisp evaluation."
      ("P")
      ";; This buffer is for notes you don't want to save,
This buffe[r];; and for Lisp evaluation."))
  (ert-info ("Paste characters with count")
    (evil-test-buffer
      ";; [T]his buffer is for notes you don't want to save,
;; and for Lisp evaluation."
      ("y2ej0")
      ";; This buffer is for notes you don't want to save,
\[;]; and for Lisp evaluation."
      ("3P")
      ";; This buffer is for notes you don't want to save,
This bufferThis bufferThis buffe[r];; and for Lisp evaluation."))
  (ert-info ("Paste characters at end-of-buffer")
    (evil-test-buffer
      ";; [T]his buffer is for notes you don't want to save,
;; and for Lisp evaluation."
      ("y2eG$")
      ";; This buffer is for notes you don't want to save,
;; and for Lisp evaluation[.]"
      ("2P")
      ";; This buffer is for notes you don't want to save,
;; and for Lisp evaluationThis bufferThis buffe[r]."))
  (ert-info ("Paste characters at end-of-buffer on empty line")
    (evil-test-buffer
      ";; [T]his buffer is for notes you don't want to save,
;; and for Lisp evaluation.\n"
      ("y2e")
      (goto-char (point-max))
      ";; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.
\[]"
      ("2P")
      ";; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.
This bufferThis buffe[r]"))
  (ert-info ("Paste lines")
    (evil-test-buffer
      ";; [T]his buffer is for notes you don't want to save,
;; and for Lisp evaluation."
      ("2yyP")
      "[;]; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.
;; This buffer is for notes you don't want to save,
;; and for Lisp evaluation."))
  (ert-info ("Paste lines with count")
    (evil-test-buffer
      ";; [T]his buffer is for notes you don't want to save,
;; and for Lisp evaluation."
      ("2yy2P")
      "[;]; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.
;; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.
;; This buffer is for notes you don't want to save,
;; and for Lisp evaluation."))
  (ert-info ("Paste lines at end-of-buffer")
    (evil-test-buffer
      ";; [T]his buffer is for notes you don't want to save,
;; and for Lisp evaluation.\n"
      ("2yy")
      (goto-char (point-max))
      ";; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.
\[]"
      ("2P")
      ";; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.
\[;]; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.
;; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.\n"))
  (ert-info ("Paste block")
    (evil-test-buffer
      "[;]; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      (define-key evil-operator-state-local-map "s" 'evil-test-square-motion)
      ("3ysP")
      "[;]; ;; This buffer is for notes you don't want to save.
;; ;; If you want to create a file, visit that file with C-x C-f,
;; ;; then enter the text in that file's own buffer."))
  (ert-info ("Paste block with count")
    (evil-test-buffer
      "[;]; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      (define-key evil-operator-state-local-map "s" 'evil-test-square-motion)
      ("3ys2P")
      "[;]; ;; ;; This buffer is for notes you don't want to save.
;; ;; ;; If you want to create a file, visit that file with C-x C-f,
;; ;; ;; then enter the text in that file's own buffer."))
  (ert-info ("Paste block with empty line")
    (evil-test-buffer
      "[;]; Above some line

;; Below some empty line"
      (define-key evil-operator-state-local-map "s" 'evil-test-square-motion)
      ("3ys2P")
      "[;]; ;; ;; Above some line
      \n\
;; ;; ;; Below some empty line"))
  (ert-info ("Paste block crossing end of buffer")
    (evil-test-buffer
      "[;]; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      (define-key evil-operator-state-local-map "s" 'evil-test-square-motion)
      ("3ysj")
      ";; This buffer is for notes you don't want to save.
\[;]; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      ("P")
      ";; This buffer is for notes you don't want to save.
\[;]; ;; If you want to create a file, visit that file with C-x C-f,
;; ;; then enter the text in that file's own buffer.
;;"))
  (ert-info ("Paste block at end-of-line")
    (evil-test-buffer
      "[;]; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      (define-key evil-operator-state-local-map "s" 'evil-test-square-motion)
      ("3ys$")
      ";; This buffer is for notes you don't want to save[.]
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      ("p")
      ";; This buffer is for notes you don't want to save.[;];
;; If you want to create a file, visit that file wi;; th C-x C-f,
;; then enter the text in that file's own buffer.  ;;")))

(ert-deftest evil-test-paste-after ()
  "Test `evil-paste-after'"
  :tags '(evil paste)
  (ert-info ("Paste characters")
    (evil-test-buffer
      ";; [T]his buffer is for notes you don't want to save,
;; and for Lisp evaluation."
      ("y2ej0")
      ";; This buffer is for notes you don't want to save,
\[;]; and for Lisp evaluation."
      ("p")
      ";; This buffer is for notes you don't want to save,
;This buffe[r]; and for Lisp evaluation."))
  (ert-info ("Paste characters with count")
    (evil-test-buffer
      ";; [T]his buffer is for notes you don't want to save,
;; and for Lisp evaluation."
      ("y2ej0")
      ";; This buffer is for notes you don't want to save,
\[;]; and for Lisp evaluation."
      ("3p")
      ";; This buffer is for notes you don't want to save,
;This bufferThis bufferThis buffe[r]; and for Lisp evaluation."))
  (ert-info ("Paste characters at end-of-buffer")
    (evil-test-buffer
      ";; [T]his buffer is for notes you don't want to save,
;; and for Lisp evaluation."
      ("y2eG$")
      ";; This buffer is for notes you don't want to save,
;; and for Lisp evaluation[.]"
      ("2p")
      ";; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.This bufferThis buffe[r]"))
  (ert-info ("Paste characters at end-of-buffer on empty line")
    (evil-test-buffer
      ";; [T]his buffer is for notes you don't want to save,
;; and for Lisp evaluation.\n"
      ("y2e")
      (goto-char (point-max))
      ";; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.
\[]"
      ("2p")
      ";; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.
This bufferThis buffe[r]"))
  (ert-info ("Paste lines")
    (evil-test-buffer
      ";; [T]his buffer is for notes you don't want to save,
;; and for Lisp evaluation."
      ("2yyp")
      ";; This buffer is for notes you don't want to save,
\[;]; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.
;; and for Lisp evaluation."))
  (ert-info ("Paste lines with count")
    (evil-test-buffer
      ";; [T]his buffer is for notes you don't want to save,
;; and for Lisp evaluation."
      ("2yy2p")
      ";; This buffer is for notes you don't want to save,
\[;]; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.
;; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.
;; and for Lisp evaluation."))
  (ert-info ("Paste lines at end-of-buffer")
    (evil-test-buffer
      ";; [T]his buffer is for notes you don't want to save,
;; and for Lisp evaluation.\n"
      ("2yy")
      (goto-char (point-max))
      ";; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.
\[]"
      ("2p")
      ";; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.

\[;]; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.
;; This buffer is for notes you don't want to save,
;; and for Lisp evaluation."))
  (ert-info ("Paste block")
    (evil-test-buffer
      "[;]; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      (define-key evil-operator-state-local-map "s" 'evil-test-square-motion)
      ("3ysp")
      ";[;]; ; This buffer is for notes you don't want to save.
;;; ; If you want to create a file, visit that file with C-x C-f,
;;; ; then enter the text in that file's own buffer."))
  (ert-info ("Paste block with count")
    (evil-test-buffer
      "[;]; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      (define-key evil-operator-state-local-map "s" 'evil-test-square-motion)
      ("3ys2p")
      ";[;]; ;; ; This buffer is for notes you don't want to save.
;;; ;; ; If you want to create a file, visit that file with C-x C-f,
;;; ;; ; then enter the text in that file's own buffer."))
  (ert-info ("Paste block with empty line")
    (evil-test-buffer
      "[;]; Above some line

;; Below some empty line"
      (define-key evil-operator-state-local-map "s" 'evil-test-square-motion)
      ("3ys2p")
      ";;; ;; ; Above some line

;;; ;; ; Below some empty line"))
  (ert-info ("Paste block crossing end of buffer")
    (evil-test-buffer
      "[;]; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      (define-key evil-operator-state-local-map "s" 'evil-test-square-motion)
      ("3ysj")
      ";; This buffer is for notes you don't want to save.
\[;]; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      ("p")
      ";; This buffer is for notes you don't want to save.
;;; ; If you want to create a file, visit that file with C-x C-f,
;;; ; then enter the text in that file's own buffer.
 ;;"))
  (ert-info ("Paste block at end-of-line")
    (evil-test-buffer
      "[;]; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      (define-key evil-operator-state-local-map "s" 'evil-test-square-motion)
      ("3ys$")
      ";; This buffer is for notes you don't want to save[.]
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      ("p")
      ";; This buffer is for notes you don't want to save.;;
;; If you want to create a file, visit that file wi;; th C-x C-f,
;; then enter the text in that file's own buffer.  ;;")))

(ert-deftest evil-test-paste-pop-before ()
  "Test `evil-paste-pop' after `evil-paste-before'"
  :tags '(evil paste)
  (ert-info ("Paste")
    (evil-test-buffer
      "[;]; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      (define-key evil-operator-state-local-map "s" 'evil-test-square-motion)
      ("y2e2yyy3sj")
      ";; This buffer is for notes you don't want to save.
\[;]; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      ("P")
      ";; This buffer is for notes you don't want to save.
\[;]; ;; If you want to create a file, visit that file with C-x C-f,
;; ;; then enter the text in that file's own buffer.
;;"))
  (ert-info ("Single pop")
    (evil-test-buffer
      "[;]; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      (define-key evil-operator-state-local-map "s" 'evil-test-square-motion)
      ("y2e2yyy3sjP\C-p")
      ";; This buffer is for notes you don't want to save.
\[;]; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."))
  (ert-info ("Two pops")
    (evil-test-buffer
      "[;]; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      (define-key evil-operator-state-local-map "s" 'evil-test-square-motion)
      ("y2e2yyy3sjP\C-p\C-p")
      ";; This buffer is for notes you don't want to save.
;; Thi[s];; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."))
  (ert-info ("Pop with count")
    (evil-test-buffer
      "[;]; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      (define-key evil-operator-state-local-map "s" 'evil-test-square-motion)
      ("y2e2yyy3sjP2\C-p")
      ";; This buffer is for notes you don't want to save.
;; Thi[s];; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."))
  (ert-info ("Single pop-next")
    (evil-test-buffer
      "[;]; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      (define-key evil-operator-state-local-map "s" 'evil-test-square-motion)
      ("y2e2yyy3sjP2\C-p\C-n")
      ";; This buffer is for notes you don't want to save.
\[;]; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."))
  (ert-info ("Pop-next with count")
    (evil-test-buffer
      "[;]; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      (define-key evil-operator-state-local-map "s" 'evil-test-square-motion)
      ("y2e2yyy3sjP\C-p\C-p2\C-n")
      ";; This buffer is for notes you don't want to save.
\[;]; ;; If you want to create a file, visit that file with C-x C-f,
;; ;; then enter the text in that file's own buffer.
;;")))

(ert-deftest evil-test-paste-pop-after ()
  "Test `evil-paste-pop' after `evil-paste-after'"
  :tags '(evil paste)
  (ert-info ("Paste")
    (evil-test-buffer
      "[;]; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      (define-key evil-operator-state-local-map "s" 'evil-test-square-motion)
      ("y2e2yyy3sj")
      ";; This buffer is for notes you don't want to save.
\[;]; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      ("p")
      ";; This buffer is for notes you don't want to save.
;[;]; ; If you want to create a file, visit that file with C-x C-f,
;;; ; then enter the text in that file's own buffer.
 ;;"))
  (ert-info ("Single pop")
    (evil-test-buffer
      "[;]; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      (define-key evil-operator-state-local-map "s" 'evil-test-square-motion)
      ("y2e2yyy3sjp\C-p")
      ";; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
\[;]; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."))
  (ert-info ("Two pops")
    (evil-test-buffer
      "[;]; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      (define-key evil-operator-state-local-map "s" 'evil-test-square-motion)
      ("y2e2yyy3sjp\C-p\C-p")
      ";; This buffer is for notes you don't want to save.
;;; Thi[s]; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."))
  (ert-info ("Pop with count")
    (evil-test-buffer
      "[;]; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      (define-key evil-operator-state-local-map "s" 'evil-test-square-motion)
      ("y2e2yyy3sjp2\C-p")
      ";; This buffer is for notes you don't want to save.
;;; Thi[s]; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."))
  (ert-info ("Single pop-next")
    (evil-test-buffer
      "[;]; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      (define-key evil-operator-state-local-map "s" 'evil-test-square-motion)
      ("y2e2yyy3sjp2\C-p\C-n")
      ";; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
\[;]; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."))
  (ert-info ("Pop-next with count")
    (evil-test-buffer
      "[;]; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      (define-key evil-operator-state-local-map "s" 'evil-test-square-motion)
      ("y2e2yyy3sjp\C-p\C-p2\C-n")
      ";; This buffer is for notes you don't want to save.
;[;]; ; If you want to create a file, visit that file with C-x C-f,
;;; ; then enter the text in that file's own buffer.
 ;;")))

(ert-deftest evil-test-paste-pop-without-undo ()
  "Test `evil-paste-pop' with undo disabled"
  :tags '(evil paste)
  (ert-info ("Pop-next with count without undo")
    (evil-test-buffer
      "[;]; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      (setq buffer-undo-list t)
      (define-key evil-operator-state-local-map "s" 'evil-test-square-motion)
      ("y2e2yyy3sjP\C-p\C-p2\C-n")
      ";; This buffer is for notes you don't want to save.
\[;]; ;; If you want to create a file, visit that file with C-x C-f,
;; ;; then enter the text in that file's own buffer.
;;")))

(ert-deftest evil-test-visual-paste ()
  "Test `evil-paste-before' and `evil-paste-after' in Visual state"
  :tags '(evil paste)
  (evil-test-buffer
    ";; This buffer is for notes you don't want to save.
;; [I]f you want to create a file, visit that file with C-x C-f."
    ("yyk")
    ";; [T]his buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f."
    ("VP")
    "[;]; If you want to create a file, visit that file with C-x C-f.
;; If you want to create a file, visit that file with C-x C-f.")
  (evil-test-buffer
    ";; [T]his buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f."
    ("yyj")
    ";; This buffer is for notes you don't want to save.
;; [I]f you want to create a file, visit that file with C-x C-f."
    ("Vp")
    ";; This buffer is for notes you don't want to save.
\[;]; This buffer is for notes you don't want to save."))

(ert-deftest evil-test-align ()
  "Test `evil-align-left', `evil-align-right' and `evil-align-center'."
  :tags '(evil operator)
  (let ((fill-column 70)
        indent-tabs-mode)
    (evil-test-buffer
      "before\n[l]ine 1\nthis is line number 2\nline number 3\nafter\n"
      (":.,+2ri" [return] (kbd "M-x") "untabify" [return])
      "before\n                                                                [l]ine 1\n                                                 this is line number 2\n                                                         line number 3\nafter\n"
      (":.,+2ri 60" [return] (kbd "M-x") "untabify" [return])
      "before\n                                                      [l]ine 1\n                                       this is line number 2\n                                               line number 3\nafter\n"
      (":.,+2le" [return] (kbd "M-x") "untabify" [return])
      "before\n[l]ine 1\nthis is line number 2\nline number 3\nafter\n"
      (":.,+2le 10" [return])
      "before\n          [l]ine 1\n          this is line number 2\n          line number 3\nafter\n"
      (":.,+2ce" [return] (kbd "M-x") "untabify" [return])
      "before\n                                [l]ine 1\n                        this is line number 2\n                            line number 3\nafter\n"
      (":.,+2ce 40" [return] (kbd "M-x") "untabify" [return])
      "before\n                 [l]ine 1\n         this is line number 2\n             line number 3\nafter\n")))

;;; Motions

(ert-deftest evil-test-forward-char ()
  "Test `evil-forward-char' motion"
  :tags '(evil motion)
  (ert-info ("Simple")
    (evil-test-buffer
      "[;]; This buffer is for notes."
      ("l")
      ";[;] This buffer is for notes."))
  (ert-info ("End of line")
    (let ((evil-cross-lines t)
          (evil-move-cursor-back t))
      (evil-test-buffer
        ";; This buffer is for notes[,]
;; and for Lisp evaluation."
        ("l")
        ";; This buffer is for notes,
\[;]; and for Lisp evaluation.")))
  (ert-info ("With count")
    (evil-test-buffer
      "[;]; This buffer is for notes."
      ("12l")
      ";; This buff[e]r is for notes."))
  (ert-info ("End of line")
    (evil-test-buffer
      ";; This buffer is for notes[.]"
      (should-error (execute-kbd-macro "l"))
      (should-error (execute-kbd-macro "10l"))))
  (ert-info ("Until end-of-line")
    (evil-test-buffer
      "[;]; This buffer is for notes."
      ("100l")
      ";; This buffer is for notes[.]"))
  (ert-info ("On empty line")
    (evil-test-buffer
      "Above some line
\[]
Below some empty line"
      (should-error (execute-kbd-macro "l"))
      (should-error (execute-kbd-macro "42l")))))

(ert-deftest evil-test-backward-char ()
  "Test `evil-backward-char' motion"
  :tags '(evil motion)
  (ert-info ("Simple")
    (evil-test-buffer
      ";; This[ ]buffer is for notes."
      ("h")
      ";; Thi[s] buffer is for notes."))
  (ert-info ("With count")
    (evil-test-buffer
      ";; This[ ]buffer is for notes."
      ("3h")
      ";; T[h]is buffer is for notes."))
  (ert-info ("Beginning of line")
    (evil-test-buffer
      "[;]; This buffer is for notes."
      (should-error (execute-kbd-macro "h"))
      (should-error (execute-kbd-macro "10h"))))
  (ert-info ("Until beginning-of-line")
    (evil-test-buffer
      ";; This[ ]buffer is for notes."
      ("100h")
      "[;]; This buffer is for notes."))
  (ert-info ("On empty line")
    (evil-test-buffer
      "Above some line
\[]
Below some empty line"
      (should-error (execute-kbd-macro "h"))
      (should-error (execute-kbd-macro "42h")))))

(ert-deftest evil-test-previous-line ()
  "Test `evil-previous-line' motion"
  :tags '(evil motion)
  (ert-info ("Simple")
    (evil-test-buffer
      ";; This buffer is for notes you don't want to save,
;; [a]nd for Lisp evaluation."
      ("k")
      ";; [T]his buffer is for notes you don't want to save,
;; and for Lisp evaluation."))
  (ert-info ("With count")
    (evil-test-buffer
      ";; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; [t]hen enter the text in that file's own buffer."
      ("2k")
      ";; [T]his buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."))
  (ert-info ("Until beginning of buffer")
    (evil-test-buffer
      ";; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; [t]hen enter the text in that file's own buffer."
      ("100k")
      ";; [T]his buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."))
  (ert-info ("At beginning of buffer")
    (evil-test-buffer
      ";; [T]his buffer is for notes you don't want to save."
      (should-error (execute-kbd-macro "k"))
      (should-error (execute-kbd-macro "42k")))))

(ert-deftest evil-test-next-line ()
  "Test `evil-next-line' motion"
  :tags '(evil motion)
  (ert-info ("Simple")
    (evil-test-buffer
      ";; [T]his buffer is for notes you don't want to save,
;; and for Lisp evaluation."
      ("j")
      ";; This buffer is for notes you don't want to save,
;; [a]nd for Lisp evaluation."))
  (ert-info ("With count")
    (evil-test-buffer
      ";; [T]his buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      ("2j")
      ";; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; [t]hen enter the text in that file's own buffer."))
  (ert-info ("Until end of buffer")
    (evil-test-buffer
      ";; [T]his buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      ("100j")
      ";; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; [t]hen enter the text in that file's own buffer."))
  (ert-info ("At end of buffer")
    (evil-test-buffer
      ";; This buffer is for notes you don't want to [s]ave."
      (should-error (execute-kbd-macro "j"))
      (should-error (execute-kbd-macro "42j")))))

(ert-deftest evil-test-beginning-of-line ()
  "Test `evil-beginning-of-line' motion"
  :tags '(evil motion)
  (evil-test-buffer
    ";; [T]his buffer is for notes you don't want to save."
    ("0")
    "[;]; This buffer is for notes you don't want to save."
    ("0")
    "[;]; This buffer is for notes you don't want to save."))

(ert-deftest evil-test-end-of-line ()
  "Test `evil-end-of-line' motion"
  :tags '(evil motion)
  (ert-info ("Simple")
    (evil-test-buffer
      ";; [T]his buffer is for notes you don't want to save."
      ("$")
      ";; This buffer is for notes you don't want to save[.]"
      ("$")
      ";; This buffer is for notes you don't want to save[.]"))
  (ert-info ("Don't delete blank lines")
    (evil-test-buffer
      "Above some line
\[]
Below some empty line"
      ("d$")
      "Above some line
\[]
Below some empty line")))

(ert-deftest evil-test-first-non-blank ()
  "Test `evil-first-non-blank' motion"
  :tags '(evil motion)
  (evil-test-buffer
    "\
  printf(\"Hello world\\n\")[;]
  return EXIT_SUCCESS;"
    ("^")
    "\
  [p]rintf(\"Hello world\\n\");
  return EXIT_SUCCESS;"
    ("j^")
    "\
  printf(\"Hello world\\n\");
  [r]eturn EXIT_SUCCESS;"))

(ert-deftest evil-test-last-non-blank ()
  "Test `evil-last-non-blank' motion"
  :tags '(evil motion)
  (evil-test-buffer
    "[i]nt main(int argc, char** argv)    \n\
{
  printf(\"Hello world\\n\");
  return EXIT_SUCCESS;
}"
    ("g_")
    "int main(int argc, char** argv[)]    \n\
{
  printf(\"Hello world\\n\");
  return EXIT_SUCCESS;
}"
    ("jjg_")
    "int main(int argc, char** argv)    \n\
{
  printf(\"Hello world\\n\")[;]
  return EXIT_SUCCESS;
}"))

(ert-deftest evil-test-goto-first-line ()
  "Test `evil-goto-first-line' motion"
  :tags '(evil motion)
  (evil-test-buffer
    "[i]nt main(int argc, char** argv)
{
  printf(\"Hello world\\n\");
  return EXIT_SUCCESS;
}"
    ("3gg")
    "int main(int argc, char** argv)
{
  [p]rintf(\"Hello world\\n\");
  return EXIT_SUCCESS;
}"
    ("gg")
    "[i]nt main(int argc, char** argv)
{
  printf(\"Hello world\\n\");
  return EXIT_SUCCESS;
}"
    ("100gg")
    "int main(int argc, char** argv)
{
  printf(\"Hello world\\n\");
  return EXIT_SUCCESS;
\[}]"))

(ert-deftest evil-test-goto-line ()
  "Test `evil-goto-line' motion"
  :tags '(evil motion)
  (evil-test-buffer
    "[i]nt main(int argc, char** argv)
{
  printf(\"Hello world\\n\");
  return EXIT_SUCCESS;
}"
    ("G")
    "int main(int argc, char** argv)
{
  printf(\"Hello world\\n\");
  return EXIT_SUCCESS;
\[}]"
    ("3G")
    "int main(int argc, char** argv)
{
  [p]rintf(\"Hello world\\n\");
  return EXIT_SUCCESS;
}"
    ("100G")
    "int main(int argc, char** argv)
{
  printf(\"Hello world\\n\");
  return EXIT_SUCCESS;
\[}]"))

(ert-deftest evil-test-operator-0 ()
  "Test motion \"0\" with an operator."
  :tags '(evil motion)
  (evil-test-buffer
    ";; [T]his buffer is for notes."
    ("d0")
    "[T]his buffer is for notes."))

;; TODO: test Visual motions and window motions
(ert-deftest evil-test-move-chars ()
  "Test `evil-move-chars'"
  :tags '(evil motion)
  (ert-info ("Simple forward")
    (evil-test-buffer
      "[i]nt main(int argc, char** argv)
{
  printf(\"Hello world\\n\");
  return EXIT_SUCCESS;
}"
      (evil-move-chars "{" 1)
      "int main(int argc, char** argv)
{[]
  printf(\"Hello world\\n\");
  return EXIT_SUCCESS;
}"
      (evil-move-chars "a-z" 1)
      "int main(int argc, char** argv)
{
  printf[(]\"Hello world\\n\");
  return EXIT_SUCCESS;
}"
      (evil-move-chars "a-z" 1)
      "int main(int argc, char** argv)
{
  printf(\"Hello[ ]world\\n\");
  return EXIT_SUCCESS;
}"))
  (ert-info ("No match")
    (evil-test-buffer
      "[i]nt main(int argc, char** argv)
{
  printf(\"Hello world\\n\");
  return EXIT_SUCCESS;
}"
      (should (eq (evil-move-chars "Q" 1) 1))))
  (ert-info ("Simple backward")
    (evil-test-buffer
      "int main(int argc, char** argv)
{
  printf(\"Hello world\\n\");
  return EXIT_SUCCESS;
\[}]")
    (evil-move-chars "*" -1)
    "int main(int argc, char[*]* argv)
{
  printf(\"Hello world\\n\");
  return EXIT_SUCCESS;
}"
    (evil-move-chars "*" -1)
    "int main(int argc, char[*]* argv)
{
  printf(\"Hello world\\n\");
  return EXIT_SUCCESS;
}")
  (ert-info ("Beginning of buffer")
    (evil-test-buffer
      "int[ ]main(int argc, char** argv)
{
  printf(\"Hello world\\n\");
  return EXIT_SUCCESS;
}"
      (should (= -1 (evil-move-chars "Q" -1))))))

(ert-deftest evil-test-forward-word-begin ()
  "Test `evil-forward-word-begin'"
  :tags '(evil motion)
  (ert-info ("Non-word")
    (evil-test-buffer
      "[;]; This buffer is for notes."
      ("w")
      ";; [T]his buffer is for notes."))
  (ert-info ("Simple")
    (evil-test-buffer
      ";; [T]his buffer is for notes."
      ("w")
      ";; This [b]uffer is for notes."))
  (ert-info ("With count")
    (evil-test-buffer
      ";; [T]his buffer is for notes."
      ("3w")
      ";; This buffer is [f]or notes."))
  (ert-info ("With count on whitespace")
    (evil-test-buffer
      ";;[ ]This buffer is for notes."
      ("3w")
      ";; This buffer [i]s for notes."))
  (ert-info ("Empty line")
    (evil-test-buffer
      "Above some line
\[]
Below some empty line"
      ("w")
      "Above some line

\[B]elow some empty line")
    (evil-test-buffer
      "[A]bove

Below some empty line"
      ("dw")
      "[]

Below some empty line"
      ("dw")
      "[]
Below some empty line"
      ("dw")
      "[B]elow some empty line")
    (evil-test-buffer
      "[A]bove

    Below some empty line with leading whitespace"
      ("dw")
      "[]

    Below some empty line with leading whitespace"
      ("dw")
      "[]
    Below some empty line with leading whitespace"
      ("dw")
      "    [B]elow some empty line")
    (evil-test-buffer
      "[A]\n"
      ("dw")
      "[]\n"))
  (ert-info ("End of buffer")
    (evil-test-buffer
      ";; [T]his buffer is for notes."
      ("100w")
      ";; This buffer is for notes[.]"
      (should-error (execute-kbd-macro "w"))
      (should-error (execute-kbd-macro "10w")))))

(ert-deftest evil-test-forward-word-end ()
  "Test `evil-forward-word-end'"
  :tags '(evil motion)
  (ert-info ("Non-word")
    (evil-test-buffer
      "[;]; This buffer is for notes."
      ("e")
      ";[;] This buffer is for notes."))
  (ert-info ("Simple")
    (evil-test-buffer
      ";; [T]his buffer is for notes."
      ("e")
      ";; Thi[s] buffer is for notes."))
  (ert-info ("With count")
    (evil-test-buffer
      ";; [T]his buffer is for notes."
      ("3e")
      ";; This buffer i[s] for notes."))
  (ert-info ("With count on whitespace")
    (evil-test-buffer
      ";;[ ]This buffer is for notes."
      ("3e")
      ";; This buffer i[s] for notes."))
  (ert-info ("Delete")
    (evil-test-buffer
      ";; This[-]buffer-is-for-notes."
      ("de")
      ";; This[-]is-for-notes."))
  (ert-info ("Empty line")
    (evil-test-buffer
      "Above some line
\[]
Below some empty line"
      ("e")
      "Above some line

Belo[w] some empty line"))
  (ert-info ("End of buffer")
    (evil-test-buffer
      ";; [T]his buffer is for notes."
      ("100e")
      ";; This buffer is for notes[.]"
      (should-error (execute-kbd-macro "e"))
      (should-error (execute-kbd-macro "10e"))))
  ;; In Vim, "de" may delete two words rather than one
  ;; if the first word is only one letter. In Evil,
  ;; "de" always deletes one word.
  (ert-info ("Delete a single-letter word")
    (evil-test-buffer
      "a [b] c"
      ("de")
      "a [ ]c")))

(ert-deftest evil-test-backward-word-begin ()
  "Test `evil-backward-word-begin'"
  :tags '(evil motion)
  (ert-info ("Simple")
    (evil-test-buffer
      ";; This buffer is for notes[.]"
      ("b")
      ";; This buffer is for [n]otes."))
  (ert-info ("With count")
    (evil-test-buffer
      ";; This buffer is for notes[.]"
      ("2b")
      ";; This buffer is [f]or notes."))
  (ert-info ("Empty line")
    (evil-test-buffer
      "Above some line
\[]
Below some empty line"
      ("b")
      "Above some [l]ine

Below some empty line"))
  (ert-info ("With count on whitespace")
    (evil-test-buffer
      ";; This buffer is for[ ]notes."
      ("2b")
      ";; This buffer [i]s for notes."))
  (ert-info ("Beginning of buffer")
    (evil-test-buffer
      ";; This buffer is for notes[.]"
      ("100b")
      "[;]; This buffer is for notes."
      (should-error (execute-kbd-macro "b"))
      (should-error (execute-kbd-macro "10b")))))

(ert-deftest evil-test-backward-word-end ()
  "Test `evil-backward-word-end'"
  :tags '(evil motion)
  (ert-info ("Simple")
    (evil-test-buffer
      ";; This buffer is for notes[.]"
      ("ge")
      ";; This buffer is for note[s]."))
  (ert-info ("With count")
    (evil-test-buffer
      ";; This buffer is for notes[.]"
      ("2ge")
      ";; This buffer is fo[r] notes."))
  (ert-info ("Empty line")
    (evil-test-buffer
      "Above some line
\[]
Below some empty line"
      ("ge")
      "Above some lin[e]

Below some empty line"))
  (ert-info ("With count on whitespace")
    (evil-test-buffer
      ";; This buffer is for[ ]notes."
      ("2ge")
      ";; This buffer i[s] for notes."))
  (ert-info ("Beginning of buffer")
    (evil-test-buffer
      ";; This buffer is for notes[.]"
      ("100ge")
      "[;]; This buffer is for notes."
      (should-error (execute-kbd-macro "ge"))
      (should-error (execute-kbd-macro "10ge")))))

(ert-deftest evil-test-move-paragraph ()
  "Test `evil-move-paragraph'"
  :tags '(evil motion)
  (ert-info ("Simple forward")
    (evil-test-buffer
      "[A]bove some line

Below some empty line"
      (should (= (evil-move-paragraph 1) 0))
      "Above some line[]

Below some empty line"
      (should (= (evil-move-paragraph 1) 0))
      "Above some line

Below some empty line[]"))
  (ert-info ("Forward with count")
    (evil-test-buffer
      "[A]bove some line

Below some empty line"
      (should (= (evil-move-paragraph 2) 0))
      "Above some line

Below some empty line[]"))
  (ert-info ("End of buffer without newline")
    (evil-test-buffer
      "[B]elow some empty line"
      (should (= (evil-move-paragraph 2) 1))
      "Below some empty line[]"
      (should (= (evil-move-paragraph 1) 1))
      "Below some empty line[]"))
  (ert-info ("End of buffer with newline")
    (evil-test-buffer
      "[B]elow some empty line\n\n"
      (should (= (evil-move-paragraph 2) 1))
      "Below some empty line[]\n\n"
      (should (= (evil-move-paragraph 1) 1))
      "Below some empty line[]\n\n"))
  (ert-info ("Simple backward")
    (evil-test-buffer
      "Above some line

Below some empty line[]"
      (should (= (evil-move-paragraph -1) 0))
      "Above some line

\[]Below some empty line"
      (should (= (evil-move-paragraph -1) 0))
      "[A]bove some line

Below some empty line"))
  (ert-info ("Backward with count")
    (evil-test-buffer
      "Above some line

Below some empty line[]"
      (should (= (evil-move-paragraph -2) 0))
      "[A]bove some line

Below some empty line"))
  (ert-info ("Beginning of buffer without newline")
    (evil-test-buffer
      "Above some line[]"
      (should (= (evil-move-paragraph -2) -1))
      "[A]bove some line"
      (should (= (evil-move-paragraph -1) -1))
      "[A]bove some line"))
  (ert-info ("Beginning of buffer with newline")
    (evil-test-buffer
      "\n\nAbove some line[]"
      (should (= (evil-move-paragraph -2) -1))
      "\n\n[A]bove some line"
      (should (= (evil-move-paragraph -1) -1))
      "\n\n[A]bove some line")))

(ert-deftest evil-test-forward-paragraph ()
  "Test `evil-forward-paragraph'"
  :tags '(evil motion)
  (ert-info ("Simple")
    (evil-test-buffer
      "[A]bove some line

Below some empty line"
      ("}")
      "Above some line
\[]
Below some empty line"))
  (ert-info ("With count")
    (evil-test-buffer
      "[A]bove some line

Below some empty line"
      ("2}")
      "Above some line

Below some empty lin[e]"))
  (ert-info ("End of buffer")
    (evil-test-buffer
      "[B]elow some empty line"
      ("100}")
      "Below some empty lin[e]"
      (should-error (execute-kbd-macro "}"))
      (should-error (execute-kbd-macro "42}"))))
  (ert-info ("End of buffer with newline")
    (evil-test-buffer
      "[B]elow some empty line\n\n"
      ("100}")
      "Below some empty line\n\n[]"
      (should-error (execute-kbd-macro "}"))
      (should-error (execute-kbd-macro "42}")))))

(ert-deftest evil-test-backward-paragraph ()
  "Test `evil-backward-paragraph'"
  :tags '(evil motion)
  (ert-info ("Simple")
    (evil-test-buffer
      "Above some line

Below some empty lin[e]"
      ("{")
      "Above some line
\[]
Below some empty line"))
  (ert-info ("With count")
    (evil-test-buffer
      "Above some line

Below some empty lin[e]"
      ("2{")
      "[A]bove some line

Below some empty line"))
  (ert-info ("Beginning of buffer")
    (evil-test-buffer
      "Above some line

Below some empty lin[e]"
      ("100{")
      "[A]bove some line

Below some empty line"
      (should-error (execute-kbd-macro "{"))
      (should-error (execute-kbd-macro "42{"))))
  (ert-info ("Beginning of buffer with newlines")
    (evil-test-buffer
      "\n\nAbove some line

Below some empty lin[e]"
      ("100{")
      "[]\n\nAbove some line

Below some empty line"
      (should-error (execute-kbd-macro "{"))
      (should-error (execute-kbd-macro "42{")))))

(ert-deftest evil-test-forward-sentence ()
  "Test `evil-forward-sentence'"
  :tags '(evil motion)
  (ert-info ("Simple")
    (evil-test-buffer
      "[;]; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.  If you want to create a file,
;; visit that file with C-x C-f.

Below some empty line."
      (")")
      ";; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.  [I]f you want to create a file,
;; visit that file with C-x C-f.

Below some empty line."
      (")")
      ";; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.  If you want to create a file,
;; visit that file with C-x C-f.
\[]
Below some empty line."
      (")")
      ";; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.  If you want to create a file,
;; visit that file with C-x C-f.

\[B]elow some empty line."))
  (ert-info ("With count")
    (evil-test-buffer
      "[;]; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.  If you want to create a file,
;; visit that file with C-x C-f.

Below some empty line."
      ("2)")
      ";; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.  If you want to create a file,
;; visit that file with C-x C-f.
\[]
Below some empty line."
      ("2)")
      ";; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.  If you want to create a file,
;; visit that file with C-x C-f.

Below some empty line[.]"))
  (ert-info ("End of buffer")
    (evil-test-buffer
      "[B]elow some empty line."
      ("100)")
      "Below some empty line[.]"
      (should-error (execute-kbd-macro ")"))
      (should-error (execute-kbd-macro "42)"))))
  (ert-info ("End of buffer with newline")
    (evil-test-buffer
      "[B]elow some empty line.\n\n"
      ("100)")
      "Below some empty line.\n\n[]"
      (should-error (execute-kbd-macro ")"))
      (should-error (execute-kbd-macro "42)")))))

(ert-deftest evil-test-backward-sentence ()
  "Test `evil-backward-sentence'"
  :tags '(evil motion)
  (ert-info ("Simple")
    (evil-test-buffer
      ";; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.  If you want to create a file,
;; visit that file with C-x C-f.

Below some empty line[.]"
      ("(")
      ";; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.  If you want to create a file,
;; visit that file with C-x C-f.

\[B]elow some empty line."
      ("(")
      ";; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.  If you want to create a file,
;; visit that file with C-x C-f.
\[]
Below some empty line."
      ("(")
      ";; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.  [I]f you want to create a file,
;; visit that file with C-x C-f.

Below some empty line."
      ("(")
      "[;]; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.  If you want to create a file,
;; visit that file with C-x C-f.

Below some empty line."))
  (ert-info ("With count")
    (evil-test-buffer
      ";; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.  If you want to create a file,
;; visit that file with C-x C-f.

Below some empty line[.]"
      ("2(")
      ";; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.  If you want to create a file,
;; visit that file with C-x C-f.
\[]
Below some empty line."
      ("2(")
      "[;]; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.  If you want to create a file,
;; visit that file with C-x C-f.

Below some empty line."))
  (ert-info ("Beginning of buffer")
    (evil-test-buffer
      ";; This buffer is for notes you don't want to save[.]"
      ("100(")
      "[;]; This buffer is for notes you don't want to save."
      (should-error (execute-kbd-macro "("))
      (should-error (execute-kbd-macro "42("))))
  (ert-info ("Beginning of buffer with newlines")
    (evil-test-buffer
      "\n\n;; This buffer is for notes you don't want to save[.]"
      ("100(")
      "[]\n\n;; This buffer is for notes you don't want to save."
      (should-error (execute-kbd-macro "("))
      (should-error (execute-kbd-macro "42(")))))

(ert-deftest evil-test-find-char ()
  "Test `evil-find-char'"
  :tags '(evil motion)
  (ert-info ("Simple")
    (evil-test-buffer
      "[;]; This buffer is for notes."
      ("fT")
      ";; [T]his buffer is for notes."))
  (ert-info ("With count")
    (evil-test-buffer
      "[;]; This buffer is for notes."
      ("2fe")
      ";; This buffer is for not[e]s."))
  (ert-info ("Repeat")
    (evil-test-buffer
      "[;]; This buffer is for notes."
      ("fe;")
      ";; This buffer is for not[e]s."))
  (ert-info ("Repeat backward")
    (evil-test-buffer
      "[;]; This buffer is for notes."
      ("2fe,")
      ";; This buff[e]r is for notes."))
  (ert-info ("No match")
    (evil-test-buffer
      "[;]; This buffer is for notes."
      (should-error (execute-kbd-macro "fL"))))
  (ert-info ("End of line")
    (let ((evil-cross-lines t))
      (evil-test-buffer
        "[;]; This buffer is for notes,
;; and for Lisp evaluation."
        ("fL")
        ";; This buffer is for notes,
;; and for [L]isp evaluation."))))

(ert-deftest evil-test-find-char-backward ()
  "Test `evil-find-char-backward'"
  :tags '(evil motion)
  (ert-info ("Simple")
    (evil-test-buffer
      ";; This buffer is for notes[.]"
      ("FT")
      ";; [T]his buffer is for notes."))
  (ert-info ("With count")
    (evil-test-buffer
      ";; This buffer is for notes[.]"
      ("2Fe")
      ";; This buff[e]r is for notes."))
  (ert-info ("Repeat")
    (evil-test-buffer
      ";; This buffer is for notes[.]"
      ("Fe;")
      ";; This buff[e]r is for notes."))
  (ert-info ("Repeat backward")
    (evil-test-buffer
      ";; This buffer is for notes[.]"
      ("2Fe,")
      ";; This buffer is for not[e]s."))
  (ert-info ("No match")
    (evil-test-buffer
      ";; This buffer is for notes[.]"
      (should-error (execute-kbd-macro "FL"))))
  (ert-info ("End of line")
    (let ((evil-cross-lines t))
      (evil-test-buffer
        ";; This buffer is for notes,
;; and for Lisp evaluation[.]"
        ("FT")
        ";; [T]his buffer is for notes,
;; and for Lisp evaluation."))))

(ert-deftest evil-test-find-char-to ()
  "Test `evil-find-char-to'"
  :tags '(evil motion)
  (ert-info ("Simple")
    (evil-test-buffer
      "[;]; This buffer is for notes."
      ("tT")
      ";;[ ]This buffer is for notes."))
  (ert-info ("With count")
    (evil-test-buffer
      "[;]; This buffer is for notes."
      ("2te")
      ";; This buffer is for no[t]es."))
  (ert-info ("Repeat")
    (evil-test-buffer
      "[;]; This buffer is for notes."
      ("tel;")
      ";; This buffer is for no[t]es."))
  (ert-info ("Repeat backward")
    (evil-test-buffer
      "[;]; This buffer is for notes."
      ("2te,")
      ";; This buffe[r] is for notes."))
  (ert-info ("No match")
    (evil-test-buffer
      "[;]; This buffer is for notes."
      (should-error (execute-kbd-macro "tL"))))
  (ert-info ("End of line")
    (let ((evil-cross-lines t))
      (evil-test-buffer
        "[;]; This buffer is for notes,
;; and for Lisp evaluation."
        ("tL")
        ";; This buffer is for notes,
;; and for[ ]Lisp evaluation."))))

(ert-deftest evil-test-find-char-to-backward ()
  "Test `evil-find-char-to-backward'"
  :tags '(evil motion)
  (ert-info ("Simple")
    (evil-test-buffer
      ";; This buffer is for notes[.]"
      ("TT")
      ";; T[h]is buffer is for notes."))
  (ert-info ("With count")
    (evil-test-buffer
      ";; This buffer is for notes[.]"
      ("2Te")
      ";; This buffe[r] is for notes."))
  (ert-info ("Repeat")
    (evil-test-buffer
      ";; This buffer is for notes[.]"
      ("Teh;")
      ";; This buffe[r] is for notes."))
  (ert-info ("Repeat backward")
    (evil-test-buffer
      ";; This buffer is for notes[.]"
      ("2Te,")
      ";; This buffer is for no[t]es."))
  (ert-info ("No match")
    (evil-test-buffer
      ";; This buffer is for notes[.]"
      (should-error (execute-kbd-macro "TL"))))
  (ert-info ("End of line")
    (let ((evil-cross-lines t))
      (evil-test-buffer
        ";; This buffer is for notes,
;; and for Lisp evaluation[.]"
        ("TT")
        ";; T[h]is buffer is for notes,
;; and for Lisp evaluation."))))

(ert-deftest evil-test-jump-item ()
  "Test `evil-jump-item'"
  :tags '(evil motion)
  (ert-info ("Simple")
    (evil-test-buffer
      "int main[(]int argc, char** argv)"
      ("%")
      "int main(int argc, char** argv[)]"
      ("%")
      "int main[(]int argc, char** argv)"))
  (ert-info ("Before parenthesis")
    (evil-test-buffer
      "[i]nt main(int argc, char** argv)"
      ("%")
      "int main(int argc, char** argv[)]"
      ("5h")
      "int main(int argc, char**[ ]argv)"
      ("%")
      "int main[(]int argc, char** argv)"))
  (ert-info ("Over several lines")
    (evil-test-buffer
      "int main(int argc, char** argv)
\[{]
  printf(\"Hello world\\n\");
  return EXIT_SUCCESS;
}"
      ("%")
      "int main(int argc, char** argv)
{
  printf(\"Hello world\\n\");
  return EXIT_SUCCESS;
\[}]"))
  (ert-info ("On line without parenthesis")
    (evil-test-buffer
      "[#]include <stdio.h>"
      (should-error (execute-kbd-macro "%")))))

(ert-deftest evil-test-unmatched-paren ()
  "Test `evil-previous-open-paren' and `evil-next-close-paren'"
  :tags '(evil motion)
  (ert-info ("Simple")
    (evil-test-buffer
      "foo ( { ( [b]ar ) baz } )"
      ("[(")
      "foo ( { [(] bar ) baz } )"
      ("])")
      "foo ( { ( bar [)] baz } )"
      ("[(")
      "foo ( { [(] bar ) baz } )"
      ("[(")
      "foo [(] { ( bar ) baz } )"
      ("f)])")
      "foo ( { ( bar ) baz } [)]"))
  (ert-info ("With count")
    (evil-test-buffer
      "foo ( { ( [b]ar ) baz } )"
      ("2[(")
      "foo [(] { ( bar ) baz } )")
    (evil-test-buffer
      "foo ( { ( [b]ar ) baz } )"
      ("2])")
      "foo ( { ( bar ) baz } [)]")))

;;; Text objects

(ert-deftest evil-test-text-object ()
  "Test `evil-define-text-object'"
  :tags '(evil text-object)
  (let ((object (evil-define-text-object nil (count)
                  (if (< count 0)
                      (list (- (point) 3) (point))
                    (list (point) (+ (point) 3))))))
    (ert-info ("Select three characters after point")
      (evil-test-buffer
        :state operator
        ";; [T]his buffer is for notes."
        (should (equal (funcall object 1) '(4 7 inclusive)))))
    (ert-info ("Select three characters before point")
      (evil-test-buffer
        :state operator
        ";; [T]his buffer is for notes."
        (should (equal (funcall object -1) '(1 4 inclusive)))))
    (ert-info ("Select three characters after selection")
      (evil-test-buffer
        ";; <Thi[s]> buffer is for notes."
        (call-interactively object)
        ";; <This b[u]>ffer is for notes."))
    (ert-info ("Select three characters before selection")
      (evil-test-buffer
        ";; <[T]his> buffer is for notes."
        (call-interactively object)
        "<[;]; This> buffer is for notes."))
    (ert-info ("Delete three characters after point")
      (evil-test-buffer
        "[;]; This buffer is for notes."
        (define-key evil-operator-state-local-map "io" object)
        ("dio")
        "[T]his buffer is for notes."))))

(ert-deftest evil-test-word-objects ()
  "Test `evil-inner-word' and `evil-a-word'"
  :tags '(evil text-object)
  (ert-info ("Select a word")
    (evil-test-buffer
      ";; [T]his buffer is for notes."
      ("viw")
      ";; <Thi[s]> buffer is for notes.")
    (evil-test-buffer
      ";; [T]his buffer is for notes."
      ("vaw")
      ";; <This[ ]>buffer is for notes."))
  (ert-info ("Select two words")
    (ert-info ("Include whitespace on this side")
      (evil-test-buffer
        ";;< Thi[s]> buffer is for notes."
        ("aw")
        ";;< This buffe[r]> is for notes.")
      (evil-test-buffer
        ";; This <[b]uffer >is for notes."
        ("aw")
        ";; <[T]his buffer >is for notes."))
    (ert-info ("Include whitespace on the other side")
      (evil-test-buffer
        ";; <This[ ]>buffer is for notes."
        ("aw")
        ";; <This buffer[ ]>is for notes.")
      (evil-test-buffer
        ";; This<[ ]buffer> is for notes."
        ("aw")
        ";;<[ ]This buffer> is for notes."))))

(ert-deftest evil-test-paragraph-objects ()
  "Test `evil-inner-paragraph' and `evil-a-paragraph'"
  :tags '(evil text-object)
  (ert-info ("Select a paragraph")
    (evil-test-buffer
      "[;]; This buffer is for notes,
;; and for Lisp evaluation.

;; This buffer is for notes,
;; and for Lisp evaluation."
      ("vap")
      "<;; This buffer is for notes,
;; and for Lisp evaluation.
\[]\n>\
;; This buffer is for notes,
;; and for Lisp evaluation.")
    (evil-test-buffer
      ";; This buffer is for notes,
\[;]; and for Lisp evaluation.

;; This buffer is for notes,
;; and for Lisp evaluation."
      ("vap")
      "<;; This buffer is for notes,
;; and for Lisp evaluation.
\[]\n>\
;; This buffer is for notes,
;; and for Lisp evaluation.")
    (evil-test-buffer
      ";; This buffer is for notes,
;; and for Lisp evaluation.
\[]
;; This buffer is for notes,
;; and for Lisp evaluation."
      ("vap")
      ";; This buffer is for notes,
;; and for Lisp evaluation.
<
;; This buffer is for notes,
;; and for Lisp evaluation[.]>"))
  (ert-info ("Select inner paragraph")
    (evil-test-buffer
      "[;]; This buffer is for notes,
;; and for Lisp evaluation.

;; This buffer is for notes,
;; and for Lisp evaluation."
      ("vip")
      "<;; This buffer is for notes,
;; and for Lisp evaluation[.]
>
;; This buffer is for notes,
;; and for Lisp evaluation.")
    (evil-test-buffer
      ";; This buffer is for notes,
\[;]; and for Lisp evaluation.

;; This buffer is for notes,
;; and for Lisp evaluation."
      ("vip")
      "<;; This buffer is for notes,
;; and for Lisp evaluation[.]
>
;; This buffer is for notes,
;; and for Lisp evaluation.")

    (evil-test-buffer
      ";; This buffer is for notes,
;; and for Lisp evaluation.
\[]
;; This buffer is for notes,
;; and for Lisp evaluation."
      ("vip")
      ";; This buffer is for notes,
;; and for Lisp evaluation.
<
;; This buffer is for notes,
;; and for Lisp evaluation[.]>")))

(ert-deftest evil-test-quote-objects ()
  "Test `evil-inner-single-quote' and `evil-a-single-quote'"
  :tags '(evil text-object)
  (ert-info ("Select text inside of '...'")
    (evil-test-buffer
      "This is 'a [t]est' for quote objects."
      ("vi'")
      "This is '<a tes[t]>' for quote objects."))
  (ert-info ("Select text including enclosing quotes")
    (evil-test-buffer
      "This is 'a [t]est' for quote objects."
      ("va'")
      "This is <'a test[']> for quote objects.")))

(ert-deftest evil-test-paren-objects ()
  "Test `evil-inner-paren', etc."
  :tags '(evil text-object)
  (ert-info ("Select inner text")
    (evil-test-buffer
      "[(]aaa)"
      (emacs-lisp-mode) ; syntax
      ("vi(")
      "(<aa[a]>)")
    (evil-test-buffer
      "(aaa[)]"
      (emacs-lisp-mode)
      ("vi(")
      "(<aa[a]>)")
    (ert-info ("Next to outer delimiter")
      (evil-test-buffer
        "([(]aaa))"
        (emacs-lisp-mode)
        ("vi(")
        "((<aa[a]>))")
      (evil-test-buffer
        "((aaa[)])"
        (emacs-lisp-mode)
        ("vi(")
        "((<aa[a]>))")))
  (ert-info ("Select parentheses inside strings")
    (evil-test-buffer
      "(aaa \"b(b[b]b)\" aa)"
      (emacs-lisp-mode)
      ("va(")
      "(aaa \"b<(bbb[)]>\" aa)"))
  (ert-info ("Break out of empty strings")
    (evil-test-buffer
      "(aaa \"bb[b]b\" aa)"
      (emacs-lisp-mode)
      ("va(")
      "<(aaa \"bbbb\" aa[)]>")))

(ert-deftest evil-test-tag-objects ()
  "Test `evil-inner-tag', etc."
  :tags '(evil text-object)
  (ert-info ("Handle nested tags")
    (evil-test-buffer
      :visual-start "{"
      :visual-end "}"
      "<p><a>f[o]o</a> bar</p>"
      ("vit")
      "<p><a>{fo[o]}</a> bar</p>"))
  (ert-info ("Break out of tags")
    (evil-test-buffer
      :visual-start "{"
      :visual-end "}"
      "<a[a]a>bbbb</aaa>"
      ("vit")
      "<aaa>{bbb[b]}</aaa>")
    (evil-test-buffer
      :visual-start "{"
      :visual-end "}"
      "<a[a]a>bbbb</aaa>"
      ("vat")
      "{<aaa>bbbb</aaa[>]}")))

(ert-deftest evil-test-paren-range ()
  "Test `evil-paren-range'"
  :tags '(evil text-object)
  (ert-info ("Select a single block")
    (ert-info ("Inside the parentheses")
      (evil-test-buffer
        "(2[3]4)"
        (should (equal (evil-paren-range 1 ?\( ?\)) '(1 6)))
        (should (equal (evil-paren-range 1 ?\( ?\) t) '(2 5)))
        (should (equal (evil-paren-range -1 ?\( ?\)) '(1 6)))
        (should (equal (evil-paren-range -1 ?\( ?\) t) '(2 5)))
        (should-not (evil-paren-range 0 ?\( ?\)))
        (should-not (evil-paren-range 0 ?\( ?\) t))))
    (ert-info ("Before opening parenthesis")
      (evil-test-buffer
        "[(]234)"
        (should (equal (evil-paren-range 1 ?\( ?\)) '(1 6)))
        (should (equal (evil-paren-range 1 ?\( ?\) t) '(2 5)))
        (should-not (evil-paren-range -1 ?\( ?\)))
        (should-not (evil-paren-range -1 ?\( ?\) t))
        (should-not (evil-paren-range 0 ?\( ?\)))
        (should-not (evil-paren-range 0 ?\( ?\) t))))
    (ert-info ("After opening parenthesis")
      (evil-test-buffer
        "([2]34)"
        (should (equal (evil-paren-range 1 ?\( ?\)) '(1 6)))
        (should (equal (evil-paren-range 1 ?\( ?\) t) '(2 5)))
        (should (equal (evil-paren-range -1 ?\( ?\)) '(1 6)))
        (should (equal (evil-paren-range -1 ?\( ?\) t) '(2 5)))
        (should-not (evil-paren-range 0 ?\( ?\)))
        (should-not (evil-paren-range 0 ?\( ?\) t))))
    (ert-info ("Before closing parenthesis")
      (evil-test-buffer
        "(234[)]"
        (should (equal (evil-paren-range 1 ?\( ?\)) '(1 6)))
        (should (equal (evil-paren-range 1 ?\( ?\) t) '(2 5)))
        (should (equal (evil-paren-range -1 ?\( ?\)) '(1 6)))
        (should (equal (evil-paren-range -1 ?\( ?\) t) '(2 5)))
        (should-not (evil-paren-range 0 ?\( ?\)))
        (should-not (evil-paren-range 0 ?\( ?\) t))))
    (ert-info ("After closing parenthesis")
      (evil-test-buffer
        "(234)[]"
        (should-not (evil-paren-range 1 ?\( ?\)))
        (should-not (evil-paren-range 1 ?\( ?\) t))
        (should (equal (evil-paren-range -1 ?\( ?\)) '(1 6)))
        (should (equal (evil-paren-range -1 ?\( ?\) t) '(2 5)))
        (should-not (evil-paren-range 0 ?\( ?\)))
        (should-not (evil-paren-range 0 ?\( ?\) t)))))
  (ert-info ("Select two blocks")
    (evil-test-buffer
      "((34567)([0]1234))"
      (should (equal (evil-paren-range 1 ?\( ?\)) '(9 16)))
      (should (equal (evil-paren-range 2 ?\( ?\)) '(1 17))))))

(ert-deftest evil-test-regexp-range ()
  "Test `evil-regexp-range'"
  :tags '(evil text-object)
  (ert-info ("Select a single block")
    (ert-info ("Inside the parentheses")
      (evil-test-buffer
        "(2[3]4)"
        (should (equal (evil-regexp-range 1 "(" ")") '(1 6)))
        (should (equal (evil-regexp-range 1 "(" ")" t) '(2 5)))
        (should (equal (evil-regexp-range -1 "(" ")") '(1 6)))
        (should (equal (evil-regexp-range -1 "(" ")" t) '(2 5)))
        (should-not (evil-regexp-range 0 "(" ")"))
        (should-not (evil-regexp-range 0 "(" ")" t))))
    (ert-info ("Before opening parenthesis")
      (evil-test-buffer
        "[(]234)"
        (should (equal (evil-regexp-range 1 "(" ")") '(1 6)))
        (should (equal (evil-regexp-range 1 "(" ")" t) '(2 5)))
        (should-not (evil-regexp-range -1 "(" ")"))
        (should-not (evil-regexp-range -1 "(" ")" t))
        (should-not (evil-regexp-range 0 "(" ")"))
        (should-not (evil-regexp-range 0 "(" ")" t))))
    (ert-info ("After opening parenthesis")
      (evil-test-buffer
        "([2]34)"
        (should (equal (evil-regexp-range 1 "(" ")") '(1 6)))
        (should (equal (evil-regexp-range 1 "(" ")" t) '(2 5)))
        (should (equal (evil-regexp-range -1 "(" ")") '(1 6)))
        (should (equal (evil-regexp-range -1 "(" ")" t) '(2 5)))
        (should-not (evil-regexp-range 0 "(" ")"))
        (should-not (evil-regexp-range 0 "(" ")" t))))
    (ert-info ("Before closing parenthesis")
      (evil-test-buffer
        "(234[)]"
        (should (equal (evil-regexp-range 1 "(" ")") '(1 6)))
        (should (equal (evil-regexp-range 1 "(" ")" t) '(2 5)))
        (should (equal (evil-regexp-range -1 "(" ")") '(1 6)))
        (should (equal (evil-regexp-range -1 "(" ")" t) '(2 5)))
        (should-not (evil-regexp-range 0 "(" ")"))
        (should-not (evil-regexp-range 0 "(" ")" t))))
    (ert-info ("After closing parenthesis")
      (evil-test-buffer
        "(234)[]"
        (should-not (evil-regexp-range 1 "(" ")"))
        (should-not (evil-regexp-range 1 "(" ")" t))
        (should (equal (evil-regexp-range -1 "(" ")") '(1 6)))
        (should (equal (evil-regexp-range -1 "(" ")" t) '(2 5)))
        (should-not (evil-regexp-range 0 "(" ")"))
        (should-not (evil-regexp-range 0 "(" ")" t)))))
  (ert-info ("Select two blocks")
    (evil-test-buffer
      "((34567)([0]1234))"
      (should (equal (evil-regexp-range 1 "(" ")") '(9 16)))
      (should (equal (evil-regexp-range 2 "(" ")") '(1 17)))))
  (ert-info ("Select a quoted block")
    (evil-test-buffer
      "'q[u]ote'"
      (should (equal (evil-regexp-range 1 "'" "'") '(1 8))))))

;;; Visual state

(defun evil-test-visual-select (selection &optional mark point)
  "Verify that TYPE is selected correctly"
  (let ((type (evil-visual-type selection)))
    (evil-visual-make-selection mark point type)
    (ert-info ("Activate region unless SELECTION is `block'")
      (cond
       ((eq selection 'block)
        (should (mark t))
        (should-not (region-active-p))
        (should-not transient-mark-mode))
       (t
        (should (mark))
        (should (region-active-p)))))
    (ert-info ("Refresh Visual markers")
      (should (= (evil-range-beginning (evil-expand (point) (mark) type))
                 evil-visual-beginning))
      (should (= (evil-range-end (evil-expand (point) (mark) type))
                 evil-visual-end))
      (should (eq (evil-visual-type) type))
      (should (eq evil-visual-direction
                  (if (< (point) (mark)) -1 1))))))

(ert-deftest evil-test-visual-refresh ()
  "Test `evil-visual-refresh'"
  :tags '(evil visual)
  (evil-test-buffer
    ";; [T]his buffer is for notes."
    (evil-visual-refresh nil nil 'inclusive)
    (should (= evil-visual-beginning 4))
    (should (= evil-visual-end 5)))
  (evil-test-buffer
    ";; [T]his buffer is for notes."
    (let ((evil-visual-region-expanded t))
      (evil-visual-refresh nil nil 'inclusive)
      (should (= evil-visual-beginning 4))
      (should (= evil-visual-end 4)))))

(ert-deftest evil-test-visual-exchange ()
  "Test `exchange-point-and-mark' in Visual character selection"
  :tags '(evil visual)
  (evil-test-buffer
    ";; <[T]his> buffer is for notes you don't want to save,
;; and for Lisp evaluation."
    ("o")
    (should (region-active-p))
    ";; <Thi[s]> buffer is for notes you don't want to save,
;; and for Lisp evaluation."))

(ert-deftest evil-test-visual-char ()
  "Test Visual character selection"
  :tags '(evil visual)
  (evil-test-buffer
    ";; [T]his buffer is for notes you don't want to save,
;; and for Lisp evaluation."
    (evil-test-visual-select 'char)
    ";; <[T]>his buffer is for notes you don't want to save,
;; and for Lisp evaluation."
    ("e")
    ";; <Thi[s]> buffer is for notes you don't want to save,
;; and for Lisp evaluation."
    ("o")
    ";; <[T]his> buffer is for notes you don't want to save,
;; and for Lisp evaluation."
    ("d")
    ";; [ ]buffer is for notes you don't want to save,
;; and for Lisp evaluation."
    ("vV")
    "<;; [ ]buffer is for notes you don't want to save,\n>\
;; and for Lisp evaluation."))

(ert-deftest evil-test-visual-line ()
  "Test Visual line selection"
  :tags '(evil visual)
  (evil-test-buffer
    ";; [T]his buffer is for notes you don't want to save,
;; and for Lisp evaluation."
    (evil-test-visual-select 'line)
    "<;; [T]his buffer is for notes you don't want to save,\n>\
;; and for Lisp evaluation."
    ("e")
    "<;; Thi[s] buffer is for notes you don't want to save,\n>\
;; and for Lisp evaluation."
    ("o")
    "<;; [T]his buffer is for notes you don't want to save,\n>\
;; and for Lisp evaluation."
    ("d")
    "[;]; and for Lisp evaluation."))

(ert-deftest evil-test-visual-block ()
  "Test Visual block selection"
  :tags '(evil visual)
  (evil-test-buffer
    "[;]; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
    (evil-test-visual-select 'block)
    "<[;]>; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
    ("jjll")
    "<;; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;;[ ]>then enter the text in that file's own buffer."
    ("O")
    ";; <This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
>[;]; then enter the text in that file's own buffer."
    ("o")
    ";;[ ]<This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
>;; then enter the text in that file's own buffer."
    ("O")
    "<[;]; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; >then enter the text in that file's own buffer."
    ("d")
    "This buffer is for notes you don't want to save.
If you want to create a file, visit that file with C-x C-f,
then enter the text in that file's own buffer."))

(ert-deftest evil-test-visual-restore ()
  "Test restoring a previous selection"
  :tags '(evil visual)
  (ert-info ("Start a characterwise selection \
if no previous selection")
    (evil-test-buffer
      ";; [T]his buffer is for notes."
      ("gv")
      ";; <[T]>his buffer is for notes."))
  (ert-info ("Restore characterwise selection")
    (evil-test-buffer
      ";; <[T]his> buffer is for notes."
      ([escape] "gv")
      ";; <[T]his> buffer is for notes."))
  (ert-info ("Restore linewise selection")
    (evil-test-buffer
      :visual line
      "<;; [T]his buffer is for notes.>"
      ([escape] "gv")
      "<;; [T]his buffer is for notes.>"))
  (ert-info ("Restore blockwise selection")
    (evil-test-buffer
      :visual block
      "<;; This buffer is for notes,
;;[ ]>and for Lisp evaluation."
      ([escape] "gv")
      "<;; This buffer is for notes,
;;[ ]>and for Lisp evaluation.")))

;;; Ex

(ert-deftest evil-test-ex-parse ()
  "Test `evil-ex-parse'"
  :tags '(evil ex)
  (should (equal (evil-ex-parse "5,2cmd arg")
                 '(evil-ex-call-command
                   (evil-ex-range
                    (evil-ex-address (string-to-number "5") nil)
                    (evil-ex-address (string-to-number "2") nil))
                   "cmd"
                   "arg")))
  (should (equal (evil-ex-parse "5,2cmd !arg")
                 '(evil-ex-call-command
                   (evil-ex-range
                    (evil-ex-address (string-to-number "5") nil)
                    (evil-ex-address (string-to-number "2") nil))
                   "cmd"
                   "!arg")))
  (should (equal (evil-ex-parse "5,2 arg")
                 '(evil-ex-call-command
                   (evil-ex-range
                    (evil-ex-address (string-to-number "5") nil)
                    (evil-ex-address (string-to-number "2") nil))
                   "arg"
                   nil))))

(ert-deftest evil-test-ex-parse-ranges ()
  "Test parsing of ranges"
  :tags '(evil ex)
  (should (equal (evil-ex-parse "%" nil 'range)
                 '(evil-ex-full-range)))
  (should (equal (evil-ex-parse "5,27" nil 'range)
                 '(evil-ex-range
                   (evil-ex-address (string-to-number "5") nil)
                   (evil-ex-address (string-to-number "27") nil))))
  (should (equal (evil-ex-parse "5;$" nil 'range)
                 '(evil-ex-range
                   (evil-ex-address (string-to-number "5") nil)
                   (evil-ex-address (evil-ex-last-line) nil))))
  (should (equal (evil-ex-parse "5,'x" nil 'range)
                 '(evil-ex-range
                   (evil-ex-address (string-to-number "5") nil)
                   (evil-ex-address (evil-ex-marker "x") nil))))
  (should (equal (evil-ex-parse "5,+" nil 'range)
                 '(evil-ex-range
                   (evil-ex-address (string-to-number "5") nil)
                   (evil-ex-address
                    nil (+ (evil-ex-signed-number (intern "+") nil))))))
  (should (equal (evil-ex-parse "5,-" nil 'range)
                 '(evil-ex-range
                   (evil-ex-address (string-to-number "5") nil)
                   (evil-ex-address
                    nil (+ (evil-ex-signed-number (intern "-") nil))))))
  (should (equal (evil-ex-parse "5;4+2-7-3+10-" nil 'range)
                 '(evil-ex-range
                   (evil-ex-address (string-to-number "5") nil)
                   (evil-ex-address
                    (string-to-number "4")
                    (+ (evil-ex-signed-number
                        (intern "+") (string-to-number "2"))
                       (evil-ex-signed-number
                        (intern "-") (string-to-number "7"))
                       (evil-ex-signed-number
                        (intern "-") (string-to-number "3"))
                       (evil-ex-signed-number
                        (intern "+") (string-to-number "10"))
                       (evil-ex-signed-number (intern "-") nil))))))
  (should (equal (evil-ex-parse ".-2;4+2-7-3+10-" nil 'range)
                 '(evil-ex-range
                   (evil-ex-address
                    (evil-ex-current-line)
                    (+ (evil-ex-signed-number
                        (intern "-") (string-to-number "2"))))
                   (evil-ex-address
                    (string-to-number "4")
                    (+ (evil-ex-signed-number
                        (intern "+") (string-to-number "2"))
                       (evil-ex-signed-number
                        (intern "-") (string-to-number "7"))
                       (evil-ex-signed-number
                        (intern "-") (string-to-number "3"))
                       (evil-ex-signed-number
                        (intern "+") (string-to-number "10"))
                       (evil-ex-signed-number
                        (intern "-") nil))))))
  (should (equal (evil-ex-parse "'a-2,$-10" nil 'range)
                 '(evil-ex-range
                   (evil-ex-address
                    (evil-ex-marker "a")
                    (+ (evil-ex-signed-number
                        (intern "-") (string-to-number "2"))))
                   (evil-ex-address
                    (evil-ex-last-line)
                    (+ (evil-ex-signed-number
                        (intern "-") (string-to-number "10")))))))
  (should (equal (evil-ex-parse ".+42" nil 'range)
                 '(evil-ex-range
                   (evil-ex-address
                    (evil-ex-current-line)
                    (+ (evil-ex-signed-number
                        (intern "+") (string-to-number "42"))))
                   nil))))

(ert-deftest evil-test-ex-goto-line ()
  "Test if :number moves point to a certain line"
  :tags '(evil ex)
  (ert-info ("Move to line")
    (evil-test-buffer
      :visual line
      "1\n 2\n [ ]3\n   4\n    5\n"
      (":4" [return])
      "1\n 2\n  3\n   [4]\n    5\n"
      (":2" [return])
      "1\n [2]\n  3\n   4\n    5\n")))

(ert-deftest evil-test-ex-repeat ()
  "Test :@: command."
  :tags '(evil ex)
  (evil-without-display
    (ert-info ("Repeat in current line")
      (evil-test-buffer
        "[a]bcdef\nabcdef\nabcdef"
        (":s/[be]/X/g" [return])
        "[a]XcdXf\nabcdef\nabcdef"
        ("jj:@:" [return])
        "aXcdXf\nabcdef\n[a]XcdXf"))
    (ert-info ("Repeat in specified line")
      (evil-test-buffer
        "[a]bcdef\nabcdef\nabcdef"
        (":s/[be]/X/g" [return])
        "[a]XcdXf\nabcdef\nabcdef"
        (":3@:" [return])
        "aXcdXf\nabcdef\n[a]XcdXf"))
    (ert-info ("Double repeat, first without then with specified line")
      (evil-test-buffer
        "[a]bcdef\nabcdef\nabcdef"
        (":s/[be]/X/" [return])
        "[a]Xcdef\nabcdef\nabcdef"
        ("jj:@:" [return] ":1@:" [return])
        "[a]XcdXf\nabcdef\naXcdef"))))

;; search
(ert-deftest evil-test-ex-substitute ()
  "Test `evil-ex-substitute'"
  :tags '(evil ex search)
  (evil-without-display
    (ert-info ("Substitute on current line")
      (evil-test-buffer
        "ABCABCABC\nABCA[B]CABC\nABCABCABC"
        (":s/BC/XYZ/" (kbd "RET"))
        "ABCABCABC\n[A]XYZABCABC\nABCABCABC"))
    (ert-info ("Substitute on whole current line")
      (evil-test-buffer
        "ABCABCABC\nABC[A]BCABC\nABCABCABC"
        (":s/BC/XYZ/g" (kbd "RET"))
        "ABCABCABC\n[A]XYZAXYZAXYZ\nABCABCABC"))
    (ert-info ("Substitute on last line")
      (evil-test-buffer
        "ABCABCABC\nABCABCABC\nABCABC[A]BC"
        (":s/BC/XYZ/" (kbd "RET"))
        "ABCABCABC\nABCABCABC\n[A]XYZABCABC"))
    (ert-info ("Substitute on whole last line")
      (evil-test-buffer
        "ABCABCABC\nABCABCABC\nABCABC[A]BC"
        (":s/BC/XYZ/g" (kbd "RET"))
        "ABCABCABC\nABCABCABC\n[A]XYZAXYZAXYZ"))
    (ert-info ("Substitute on range")
      (evil-test-buffer
        "ABCABCABC\nQRT\nABC[A]BCABC\nABCABCABC"
        (":1,3s/BC/XYZ/" (kbd "RET"))
        "AXYZABCABC\nQRT\n[A]XYZABCABC\nABCABCABC"))
    (ert-info ("Substitute whole lines on range")
      (evil-test-buffer
        "ABCABCABC\nQRT\nABC[A]BCABC\nABCABCABC"
        (":1,3s/BC/XYZ/g" (kbd "RET"))
        "AXYZAXYZAXYZ\nQRT\n[A]XYZAXYZAXYZ\nABCABCABC"))
    (ert-info ("Substitute on whole current line confirm")
      (evil-test-buffer
        "ABCABCABC\nABC[A]BCABC\nABCABCABC"
        (":s/BC/XYZ/gc" (kbd "RET") "yny")
        "ABCABCABC\n[A]XYZABCAXYZ\nABCABCABC"))
    (ert-info ("Substitute on range confirm")
      (evil-test-buffer
        "ABCABCABC\nQRT\nABC[A]BCABC\nABCABCABC"
        (":1,3s/BC/XYZ/c" (kbd "RET") "yn")
        "[A]XYZABCABC\nQRT\nABCABCABC\nABCABCABC"))
    (ert-info ("Substitute whole lines on range with other delim")
      (evil-test-buffer
        "A/CA/CA/C\nQRT\nA/C[A]/CA/C\nA/CA/CA/C"
        (":1,3s,/C,XYZ,g" (kbd "RET"))
        "AXYZAXYZAXYZ\nQRT\n[A]XYZAXYZAXYZ\nA/CA/CA/C"))
    (ert-info ("Substitute on whole buffer, smart case")
      (evil-test-buffer
        "[A]bcAbcAbc\naBcaBcaBc\nABCABCABC\nabcabcabc"
        (":%s/bc/xy/g" (kbd "RET"))
        "AxyAxyAxy\naXyaXyaXy\nAXYAXYAXY\n[a]xyaxyaxy"))))

(ert-deftest evil-test-ex-substitute-replacement ()
  "Test `evil-ex-substitute' with special replacements."
  :tags '(evil ex search)
  (ert-info ("Substitute upper first on first match in line")
    (evil-test-buffer
      "[x]xx foo bar foo bar foo bar"
      (":s/\\(foo\\|bar\\)/\\u\\1" [return])
      "[x]xx Foo bar foo bar foo bar"))
  (ert-info ("Substitute upper first on first match in line with confirm")
    (evil-test-buffer
      "[x]xx foo bar foo bar foo bar"
      (":s/\\(foo\\|bar\\)/\\u\\1/c" [return] "y")
      "[x]xx Foo bar foo bar foo bar"))
  (ert-info ("Substitute upper first on whole line")
    (evil-test-buffer
      "[x]xx foo bar foo bar foo bar"
      (":s/\\(foo\\|bar\\)/\\u\\1/g" [return])
      "[x]xx Foo Bar Foo Bar Foo Bar"))
  (ert-info ("Substitute upper first on whole line")
    (evil-test-buffer
      "[x]xx foo bar foo bar foo bar"
      (":s/\\(foo\\|bar\\)/\\u\\1/gc" [return] "yynyyn")
      "[x]xx Foo Bar foo Bar Foo bar"))
  (ert-info ("Substitute upper/lower on first match in line")
    (evil-test-buffer
      "[x]xx foo BAR foo BAR foo BAR"
      (":s/\\(f[[:alpha:]]*\\>\\)\\s-*\\(b[[:alpha:]]*\\>\\)/\\L\\2_\\e\\U\\1" [return])
      "[x]xx bar_FOO foo BAR foo BAR"))
  (ert-info ("Substitute upper/lower on first match in line with confirm")
    (evil-test-buffer
      "[x]xx foo BAR foo BAR foo BAR"
      (":s/\\(f[[:alpha:]]*\\>\\)\\s-*\\(b[[:alpha:]]*\\>\\)/\\L\\2_\\e\\U\\1/c" [return] "y")
      "[x]xx bar_FOO foo BAR foo BAR"))
  (ert-info ("Substitute upper/lower on whole line")
    (evil-test-buffer
      "[x]xx foo BAR foo BAR foo BAR"
      (":s/\\(f[[:alpha:]]*\\>\\)\\s-*\\(b[[:alpha:]]*\\>\\)/\\L\\2_\\e\\U\\1/g" [return])
      "[x]xx bar_FOO bar_FOO bar_FOO"))
  (ert-info ("Substitute upper/lower on whole line")
    (evil-test-buffer
      "[x]xx foo BAR foo BAR foo BAR"
      (":s/\\(f[[:alpha:]]*\\>\\)\\s-*\\(b[[:alpha:]]*\\>\\)/\\L\\2_\\e\\U\\1/gc" [return] "yny")
      "[x]xx bar_FOO foo BAR bar_FOO")))

(ert-deftest evil-test-ex-regex-without-case ()
  "Test `evil-ex-regex-without-case'"
  :tags '(evil ex search)
  (should (equal (evil-ex-regex-without-case "cdeCDE")
                 "cdeCDE"))
  (should (equal (evil-ex-regex-without-case "\\ccde\\CCDE")
                 "cdeCDE"))
  (should (equal (evil-ex-regex-without-case "\\\\ccde\\\\CCDE")
                 "\\\\ccde\\\\CCDE"))
  (should (equal (evil-ex-regex-without-case "\\\\\\ccde\\\\\\CCDE")
                 "\\\\cde\\\\CDE")))

(ert-deftest evil-test-ex-regex-case ()
  "Test `evil-ex-regex-case'"
  :tags '(evil ex search)
  (should (equal (evil-ex-regex-case "cde" 'smart) 'insensitive))
  (should (equal (evil-ex-regex-case "cDe" 'smart) 'sensitive))
  (should (equal (evil-ex-regex-case "cde" 'sensitive) 'sensitive))
  (should (equal (evil-ex-regex-case "cde" 'insensitive) 'insensitive))
  (should (equal (evil-ex-regex-case "\\ccde" 'smart) 'insensitive))
  (should (equal (evil-ex-regex-case "\\cCde" 'smart) 'insensitive))
  (should (equal (evil-ex-regex-case "\\Ccde" 'smart) 'sensitive))
  (should (equal (evil-ex-regex-case "\\CCde" 'smart) 'sensitive))
  (should (equal (evil-ex-regex-case "\\ccd\\Ce" 'smart) 'insensitive))
  (should (equal (evil-ex-regex-case "\\cCd\\Ce" 'smart) 'insensitive))
  (should (equal (evil-ex-regex-case "\\Ccd\\ce" 'smart) 'sensitive))
  (should (equal (evil-ex-regex-case "\\CCd\\ce" 'smart) 'sensitive)))

(ert-deftest evil-test-ex-search ()
  "Test evil internal search."
  :tags '(evil ex search)
  (evil-without-display
    (evil-select-search-module 'evil-search-module 'evil-search)
    (ert-info ("Test smart case insensitive")
      (evil-test-buffer
        "[s]tart you YOU You you YOU You"
        ("/you" [return])
        "start [y]ou YOU You you YOU You"
        ("n")
        "start you [Y]OU You you YOU You"
        ("n")
        "start you YOU [Y]ou you YOU You"
        ("n")
        "start you YOU You [y]ou YOU You"))
    (ert-info ("Test smart case sensitive")
      (evil-test-buffer
        "[s]tart you YOU You you YOU You"
        ("/You" [return])
        "start you YOU [Y]ou you YOU You"
        ("n")
        "start you YOU You you YOU [Y]ou"))
    (ert-info ("Test insensitive")
      (evil-test-buffer
        "[s]tart you YOU You you YOU You"
        ("/\\cyou" [return])
        "start [y]ou YOU You you YOU You"
        ("n")
        "start you [Y]OU You you YOU You"
        ("n")
        "start you YOU [Y]ou you YOU You"
        ("n")
        "start you YOU You [y]ou YOU You"))
    (ert-info ("Test sensitive")
      (evil-test-buffer
        "[s]tart you YOU You you YOU You"
        ("/\\Cyou" [return])
        "start [y]ou YOU You you YOU You"
        ("n")
        "start you YOU You [y]ou YOU You"))))

(ert-deftest evil-test-ex-search-offset ()
  "Test search offsets."
  :tags '(evil ex search)
  (evil-without-display
    (evil-select-search-module 'evil-search-module 'evil-search)
    (ert-info ("Test line offsets")
      (evil-test-buffer
        "[f]oo foo\nbar bar\nbaz baz\nAnother line\nAnd yet another line"
        ("/bar/2")
        "foo foo\nbar bar\nbaz baz\n[A]nother line\nAnd yet another line"
        ("?bar?-")
        "[f]oo foo\nbar bar\nbaz baz\nAnother line\nAnd yet another line"
        ("/r bar/")
        "foo foo\nba[r] bar\nbaz baz\nAnother line\nAnd yet another line"))
    (ert-info ("Test end offsets")
      (evil-test-buffer
        "[f]oo foo\nbar bar\nbaz baz\nAnother line\nAnd yet another line"
        ("/bar/e")
        "foo foo\nba[r] bar\nbaz baz\nAnother line\nAnd yet another line"
        ("/baz/e+2")
        "foo foo\nbar bar\nbaz [b]az\nAnother line\nAnd yet another line"
        ("/line/e-1")
        "foo foo\nbar bar\nbaz baz\nAnother li[n]e\nAnd yet another line"))
    (ert-info ("Test begin offsets")
      (evil-test-buffer
        "[f]oo foo\nbar bar\nbaz baz\nAnother line\nAnd yet another line"
        ("/bar/b")
        "foo foo\n[b]ar bar\nbaz baz\nAnother line\nAnd yet another line"
        ("/baz/b+2")
        "foo foo\nbar bar\nba[z] baz\nAnother line\nAnd yet another line"
        ("/line/b-")
        "foo foo\nbar bar\nbaz baz\nAnother[ ]line\nAnd yet another line"))
    (ert-info ("Test search-next with offset")
      (evil-test-buffer
        "[f]oo foo\nbar bar\nbaz baz\nAnother line\nAnd yet another line"
        ("/ ba/+1" [return])
        "foo foo\nbar bar\n[b]az baz\nAnother line\nAnd yet another line"
        ("n")
        "foo foo\nbar bar\nbaz baz\n[A]nother line\nAnd yet another line"))))

(ert-deftest evil-test-ex-search-pattern-offset ()
  "Test pattern offsets."
  :tags '(evil ex search)
  (evil-without-display
    (evil-select-search-module 'evil-search-module 'evil-search)
    (ert-info ("Test simple pattern offsets")
      (evil-test-buffer
        "[f]oo foo\nbar bar\nfoo foo\nbaz baz\nAnother line\nAnd yet another line"
        ("/bar/;/foo" [return])
        "foo foo\nbar bar\n[f]oo foo\nbaz baz\nAnother line\nAnd yet another line"))
    (ert-info ("Test simple pattern offsets in backward direction")
      (evil-test-buffer
        "[f]oo foo\nbar bar\nfoo foo\nbaz baz\nAnother line\nAnd yet another line"
        ("/bar/;?foo" [return])
        "foo [f]oo\nbar bar\nfoo foo\nbaz baz\nAnother line\nAnd yet another line"))
    (ert-info ("Ensure second pattern is used for search repeat")
      (evil-test-buffer
        "[f]oo foo\nbar bar\nfoo foo\nbaz baz\nAnother line\nAnd yet another line"
        ("/bar/;?foo" [return] "n")
        "foo foo\nbar bar\n[f]oo foo\nbaz baz\nAnother line\nAnd yet another line"))))

(ert-deftest evil-test-ex-search-repeat ()
  "Test repeat of search."
  :tags '(evil ex search)
  (evil-without-display
    (evil-select-search-module 'evil-search-module 'evil-search)
    (ert-info ("Test repeat of simple pattern")
      (evil-test-buffer
        "[f]oo foo\nbar bar\nbaz baz\nAnother line\nAnd yet another line"
        ("/bar" [return] "/" [return])
        "foo foo\nbar [b]ar\nbaz baz\nAnother line\nAnd yet another line"))
    (ert-info ("Test repeat of simple pattern with new offset")
      (evil-test-buffer
        "[f]oo foo\nbar bar\nbaz baz\nAnother line\nAnd yet another line"
        ("/bar" [return] "//e" [return])
        "foo foo\nbar ba[r]\nbaz baz\nAnother line\nAnd yet another line"))
    (ert-info ("Test repeat of pattern with offset")
      (evil-test-buffer
        "[f]oo foo\nbar bar\nbaz baz\nAnother line\nAnd yet another line"
        ("/bar/e" [return] "/" [return])
        "foo foo\nbar ba[r]\nbaz baz\nAnother line\nAnd yet another line"))
    (ert-info ("Test repeat of pattern with offset without offset")
      (evil-test-buffer
        "[f]oo foo\nbar bar\nbaz baz\nAnother line\nAnd yet another line"
        ("/bar/e" [return] "//" [return])
        "foo foo\nbar [b]ar\nbaz baz\nAnother line\nAnd yet another line"))
    (ert-info ("Test repeat of pattern with offset with new offset")
      (evil-test-buffer
        "[f]oo foo\nbar bar\nbaz baz\nAnother line\nAnd yet another line"
        ("/bar/e" [return] "//b+1" [return])
        "foo foo\nbar b[a]r\nbaz baz\nAnother line\nAnd yet another line"))))

(ert-deftest evil-test-read ()
  "Test of `evil-read'"
  :tags '(evil ex)
  (evil-without-display
    (ert-info ("Test insertion of file with trailing newline")
      (evil-with-temp-file name
          "temp file 1\ntemp file 2\n"
        (ert-info ("At first line")
          (evil-test-buffer
            "[l]ine 1\nline 2"
            ((vconcat ":read " name [return]))
            "line 1\n[t]emp file 1\ntemp file 2\nline 2"))
        (ert-info ("At last line")
          (evil-test-buffer
            "line 1\n[l]ine 2"
            ((vconcat ":read " name [return]))
            "line 1\nline 2\n[t]emp file 1\ntemp file 2\n"))
        (ert-info ("After specified line number")
          (evil-test-buffer
            "[l]ine 1\nline 2\nline 3\nline 4\line 5"
            ((vconcat ":3read " name [return]))
            "line 1\nline 2\nline 3\n[t]emp file 1\ntemp file 2\nline 4\line 5"))
        (ert-info ("After specified line 0")
          (evil-test-buffer
            "line 1\nline [2]\nline 3\nline 4\line 5"
            ((vconcat ":0read " name [return]))
            "[t]emp file 1\ntemp file 2\nline 1\nline 2\nline 3\nline 4\line 5"))))
    (ert-info ("Test insertion of file without trailing newline")
      (evil-with-temp-file name
          "temp file 1\ntemp file 2"
        (evil-test-buffer
          "[l]ine 1\nline 2"
          ((vconcat ":read " name [return]))
          "line 1\n[t]emp file 1\ntemp file 2\nline 2")))
    (ert-info ("Test insertion of shell command")
      (ert-info ("with space")
        (evil-test-buffer
          "[l]line 1\nline 2"
          (":read !echo cmd line 1" [return])
          "line 1\n[c]md line 1\nline 2"))
      (ert-info ("without space")
        (evil-test-buffer
          "[l]line 1\nline 2"
          (":read!echo cmd line 1" [return])
          "line 1\n[c]md line 1\nline 2")))
    (ert-info ("Test insertion of shell command without trailing newline")
      (ert-info ("with space")
        (evil-test-buffer
          "[l]line 1\nline 2"
          (":read !echo -n cmd line 1" [return])
          "line 1\n[c]md line 1\nline 2"))
      (ert-info ("without space")
        (evil-test-buffer
          "[l]line 1\nline 2"
          (":read!echo -n cmd line 1" [return])
          "line 1\n[c]md line 1\nline 2")))))

;;; Utilities

(ert-deftest evil-test-parser ()
  "Test `evil-parser'"
  (let ((grammar '((number "[0-9]+" #'string-to-number)
                   (plus "\\+" #'intern)
                   (minus "-" #'intern)
                   (operator
                    plus
                    minus)
                   (inc
                    (number #'(lambda (n) (1+ n))))
                   (expr
                    (number operator number)
                    ("2" #'"1+1"))
                   (epsilon nil))))
    (ert-info ("Nothing")
      (should (equal (evil-parser "1+2" nil grammar t)
                     nil))
      (should (equal (evil-parser "1+2" nil grammar)
                     '(nil . "1+2")))
      (should (equal (evil-parser "1+2" 'epsilon grammar t)
                     nil))
      (should (equal (evil-parser "1+2" 'epsilon grammar)
                     '(nil . "1+2"))))
    (ert-info ("Strings")
      (should (equal (evil-parser "1" 'number grammar t)
                     '((string-to-number "1"))))
      (should (equal (evil-parser "11" 'number grammar)
                     '((string-to-number "11") . ""))))
    (ert-info ("Sequences")
      (should (equal (evil-parser "1" '(number) grammar t)
                     '((list (string-to-number "1")))))
      (should (equal (evil-parser "1+2" '(number operator number) grammar t)
                     '((list
                        (string-to-number "1")
                        (intern "+")
                        (string-to-number "2"))))))
    (ert-info ("Symbols")
      (should (equal (evil-parser "+" 'plus grammar t)
                     '((intern "+"))))
      (should (equal (evil-parser "+" 'operator grammar t)
                     '((intern "+"))))
      (should (equal (evil-parser "1" 'number grammar t)
                     '((string-to-number "1")))))
    (ert-info ("Whitespace")
      (should (equal (evil-parser " 1" 'number grammar t)
                     '((string-to-number "1")))))
    (ert-info ("One or more")
      (should (equal (evil-parser "1 2 3" '(+ number) grammar t)
                     '((list
                        (string-to-number "1")
                        (string-to-number "2")
                        (string-to-number "3")))))
      (should (equal (evil-parser "1 2 3" '(* number) grammar t)
                     '((list
                        (string-to-number "1")
                        (string-to-number "2")
                        (string-to-number "3")))))
      (should (equal (evil-parser "1 2 3" '(\? number) grammar)
                     '((string-to-number "1") . " 2 3")))
      (should (equal (evil-parser "1 2 3" '(\? number number) grammar)
                     '((list
                        (string-to-number "1")
                        (string-to-number "2"))
                       . " 3")))
      (should (equal (evil-parser "1 2 3" '(number (\? number)) grammar)
                     '((list
                        (string-to-number "1")
                        (string-to-number "2"))
                       . " 3")))
      (should (equal (evil-parser "1 2 3" '(number (\? number number)) grammar)
                     '((list
                        (string-to-number "1")
                        (list
                         (string-to-number "2")
                         (string-to-number "3")))
                       . "")))
      (should (equal (evil-parser "1 a 3" '(number (\? number)) grammar)
                     '((list
                        (string-to-number "1")
                        nil)
                       . " a 3"))))
    (ert-info ("Lookahead")
      (should (equal (evil-parser "foobar" '("foo" (& "bar")) grammar)
                     '((list "foo") . "bar")))
      (should (equal (evil-parser "foobar" '("foo" (! "bar")) grammar)
                     nil))
      (should (equal (evil-parser "foobar" '("foo" (& "baz")) grammar)
                     nil))
      (should (equal (evil-parser "foobar" '("foo" (! "baz")) grammar)
                     '((list "foo") . "bar"))))
    (ert-info ("Semantic actions")
      (should (equal (evil-parser "1" 'inc grammar t)
                     '((funcall (lambda (n)
                                  (1+ n))
                                (string-to-number "1")))))
      (should (equal (evil-parser "1+1" 'expr grammar t)
                     '((list
                        (string-to-number "1")
                        (intern "+")
                        (string-to-number "1")))))
      (should (equal (evil-parser "2" 'expr grammar t)
                     '((list (string-to-number "1")
                             (intern "+")
                             (string-to-number "1"))))))))

(ert-deftest evil-test-properties ()
  "Test `evil-get-property' and `evil-put-property'"
  :tags '(evil util)
  (let (alist)
    (ert-info ("Set properties")
      (evil-put-property 'alist 'wibble :foo t)
      (should (equal alist '((wibble . (:foo t)))))
      (evil-put-property 'alist 'wibble 'bar nil)
      (should (equal alist '((wibble . (:foo t :bar nil)))))
      (evil-put-property 'alist 'wobble :foo nil :bar nil :baz t)
      (should (equal alist '((wobble . (:foo nil :bar nil :baz t))
                             (wibble . (:foo t :bar nil))))))
    (ert-info ("Get properties")
      (should (evil-get-property alist 'wibble 'foo))
      (should-not (evil-get-property alist 'wibble :bar))
      (should-not (evil-get-property alist 'wobble :foo))
      (should-not (evil-get-property alist 'wibble :baz))
      (should (equal (evil-get-property alist t :foo)
                     '((wibble . t) (wobble . nil))))
      (should (equal (evil-get-property alist t :bar)
                     '((wibble . nil) (wobble . nil))))
      (should (equal (evil-get-property alist t :baz)
                     '((wobble . t)))))))

(ert-deftest evil-test-filter-list ()
  "Test `evil-filter-list'"
  :tags '(evil util)
  (ert-info ("Return filtered list")
    (should (equal (evil-filter-list #'null '(nil)) nil))
    (should (equal (evil-filter-list #'null '(nil 1)) '(1)))
    (should (equal (evil-filter-list #'null '(nil 1 2 nil)) '(1 2)))
    (should (equal (evil-filter-list #'null '(nil nil 1)) '(1)))
    (should (equal (evil-filter-list #'null '(nil 1 nil 2 nil 3))
                   '(1 2 3))))
  (ert-info ("Remove matches by side-effect when possible")
    (let (list)
      (setq list '(1 nil))
      (evil-filter-list #'null list)
      (should (equal list '(1)))

      (setq list '(1 nil nil))
      (evil-filter-list #'null list)
      (should (equal list '(1)))

      (setq list '(1 nil nil 2))
      (evil-filter-list #'null list)
      (should (equal list '(1 2)))

      (setq list '(1 nil 2 nil 3))
      (evil-filter-list #'null list)
      (should (equal list '(1 2 3))))))

(ert-deftest evil-test-concat-lists ()
  "Test `evil-concat-lists' and `evil-concat-alists'"
  :tags '(evil util)
  (ert-info ("Remove duplicates across lists")
    (should (equal (evil-concat-lists
                    nil '(a b) '(b c))
                   '(a b c))))
  (ert-info ("Remove duplicates inside lists")
    (should (equal (evil-concat-lists
                    '(a a b) nil '(b c) nil)
                   '(a b c))))
  (ert-info ("Remove duplicate associations")
    (should (equal (evil-concat-alists
                    '((a . b)) '((a . c)))
                   '((a . c))))
    (should-not (equal (evil-concat-lists
                        '((a . b)) '((a . c)))
                       '((a . b))))))

(ert-deftest evil-test-sort ()
  "Test `evil-sort' and `evil-swap'"
  :tags '(evil util)
  (let (a b c d)
    (ert-info ("Two elements")
      (setq a 2 b 1)
      (evil-sort a b)
      (should (= a 1))
      (should (= b 2))
      (evil-swap a b)
      (should (= a 2))
      (should (= b 1)))
    (ert-info ("Three elements")
      (setq a 3 b 1 c 2)
      (evil-sort a b c)
      (should (= a 1))
      (should (= b 2))
      (should (= c 3)))
    (ert-info ("Four elements")
      (setq a 4 b 3 c 2 d 1)
      (evil-sort a b c d)
      (should (= a 1))
      (should (= b 2))
      (should (= c 3))
      (should (= d 4)))))

(ert-deftest evil-test-read-key ()
  "Test `evil-read-key'"
  :tags '(evil util)
  (let ((unread-command-events '(?A)))
    (ert-info ("Prevent downcasing in `this-command-keys'")
      (should (eq (evil-read-key) ?A))
      (should (equal (this-command-keys) "A")))))

(ert-deftest evil-test-extract-count ()
  "Test `evil-extract-count'"
  :tags '(evil util)
  (evil-test-buffer
    (ert-info ("Exact without count")
      (should (equal (evil-extract-count "x")
                     (list nil 'evil-delete-char "x" nil)))
      (should (equal (evil-extract-count "g0")
                     (list nil 'evil-beginning-of-visual-line "g0" nil))))

    (ert-info ("Exact with count")
      (should (equal (evil-extract-count "420x")
                     (list 420 'evil-delete-char "x" nil)))
      (should (equal (evil-extract-count (vconcat "420" [M-right]))
                     (list 420 (key-binding [M-right]) (vconcat [M-right]) nil)))
      (should (equal (evil-extract-count "2301g0")
                     (list 2301 'evil-beginning-of-visual-line "g0" nil))))

    (ert-info ("Extra elements without count")
      (should (equal (evil-extract-count "xAB")
                     (list nil 'evil-delete-char "x" "AB")))
      (should (equal (evil-extract-count "g0CD")
                     (list nil 'evil-beginning-of-visual-line "g0" "CD"))))

    (ert-info ("Extra elements with count")
      (should (equal (evil-extract-count "420xAB")
                     (list 420 'evil-delete-char "x" "AB")))
      (should (equal (evil-extract-count "2301g0CD")
                     (list 2301 'evil-beginning-of-visual-line "g0" "CD"))))

    (ert-info ("Exact \"0\" count")
      (should (equal (evil-extract-count "0")
                     (list nil 'evil-digit-argument-or-evil-beginning-of-line
                           "0" nil))))

    (ert-info ("Extra elements and \"0\"")
      (should (equal (evil-extract-count "0XY")
                     (list nil 'evil-digit-argument-or-evil-beginning-of-line
                           "0" "XY"))))

    (ert-info ("Count only")
      (should-error (evil-extract-count "1230")))

    (ert-info ("Unknown command")
      (should-error (evil-extract-count "°"))
      (should-error (evil-extract-count "12°")))))

;;; Advice

(ert-deftest evil-test-eval-last-sexp ()
  "Test advised `evil-last-sexp'"
  :tags '(evil advice)
  (ert-info ("Normal state")
    (evil-test-buffer
      "(+ 1 (+ 2 3[)])"
      ("1" (kbd "C-x C-e"))
      "(+ 1 (+ 2 35[)])"))
  (ert-info ("Insert state")
    (evil-test-buffer
      "(+ 1 (+ 2 3[)])"
      ("i" (kbd "C-u") (kbd "C-x C-e") [escape])
      "(+ 1 (+ 2 3[3]))"))
  (ert-info ("Emacs state")
    (evil-test-buffer
      "(+ 1 (+ 2 3[)])"
      ((kbd "C-z") (kbd "C-u") (kbd "C-x C-e"))
      "(+ 1 (+ 2 33[)])")))

;;; ESC

(ert-deftest evil-test-esc-count ()
  "Test if prefix-argument is transfered for key sequences with meta-key"
  :tags '(evil esc)
  (unless noninteractive
    (ert-info ("Test M-<right>")
      (evil-test-buffer
        "[A]BC DEF GHI JKL MNO"
        ("3" (kbd "ESC <right>"))
        "ABC DEF GHI[ ]JKL MNO"))
    (ert-info ("Test shell-command")
      (evil-test-buffer
        "[A]BC DEF GHI JKL MNO"
        ("1" (kbd "ESC !") "echo TEST" [return])
        "[T]EST\nABC DEF GHI JKL MNO"))))

(when (or evil-tests-profiler evil-tests-run)
  (evil-tests-initialize))

(provide 'evil-tests)

;;; evil-tests.el ends here
