;; evil-tests.el --- unit tests for Evil -*- coding: utf-8 -*-

;; This file is for developers. It runs some unit tests on Evil.
;; To load it, add the following lines to .emacs:
;;
;;     (setq evil-tests-run t) ; run tests immediately
;;     (global-set-key [f12] 'evil-tests-run) ; hotkey
;;     (require 'evil-tests)
;;
;; This file is NOT part of Evil itself.

(require 'ert)
(require 'evil)

(defvar evil-tests-run nil
  "Run Evil tests.")

(defun evil-tests-run (&optional arg)
  "Run Evil tests."
  (interactive '(t))
  (if arg (ert-run-tests-interactively '(tag evil))
    (ert-run-tests-batch '(tag evil))))

(defmacro evil-test-buffer (&rest body)
  "Execute BODY in a temporary buffer.
The buffer contains the familiar *scratch* message,
and `evil-local-mode' is enabled."
  (declare (indent defun)
           (debug t))
  `(let ((kill-ring kill-ring)
         (kill-ring-yank-pointer kill-ring-yank-pointer)
         x-select-enable-clipboard
         message-log-max)
     (save-window-excursion
       (with-temp-buffer
         (switch-to-buffer-other-window (current-buffer))
         (buffer-enable-undo)
         (evil-local-mode 1)
         (insert ";; This buffer is for notes you don't want to save, \
and for Lisp evaluation.\n;; If you want to create a file, visit \
that file with C-x C-f,\n;; then enter the text in that file's own \
buffer.\n")
         (goto-char (point-min))
         ,@body))))

;;; States

(defun evil-test-local-mode-enabled ()
  "Verify that `evil-local-mode' is enabled properly"
  (ert-info ("Set the mode variable to t")
    (should (eq evil-local-mode t)))
  (ert-info ("Refresh `emulation-mode-map-alist'")
    (should (memq 'evil-mode-map-alist emulation-mode-map-alists)))
  (ert-info ("Refresh the modeline")
    (should (memq 'evil-modeline-tag global-mode-string)))
  (ert-info ("Create a buffer-local value for `evil-mode-map-alist'")
    (should (assq 'evil-mode-map-alist (buffer-local-variables))))
  (ert-info ("Initialize buffer-local keymaps")
    (should (assq 'evil-normal-state-local-map (buffer-local-variables)))
    (should (keymapp evil-normal-state-local-map))
    (should (assq 'evil-emacs-state-local-map (buffer-local-variables)))
    (should (keymapp evil-emacs-state-local-map)))
  (ert-info ("Refresh buffer-local entries in `evil-mode-map-alist'")
    (should (rassq evil-normal-state-local-map evil-mode-map-alist))
    (should (rassq evil-emacs-state-local-map evil-mode-map-alist)))
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
    (dolist (state (mapcar 'car evil-states-alist) t)
      (should-not (symbol-value (evil-state-property state :mode)))
      (should-not (memq (symbol-value (evil-state-property state :keymap))
                        (current-active-maps)))
      (should-not (symbol-value (evil-state-property state :local-mode)))
      (should-not (memq (symbol-value (evil-state-property state :local-keymap))
                        (current-active-maps)))
      (dolist (map (evil-state-auxiliary-keymaps state))
        (should-not (memq map (current-active-maps)))))))

(ert-deftest evil-test-toggle-local-mode ()
  "Toggle `evil-local-mode'"
  :tags '(evil)
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
          keymap (symbol-value (evil-state-property
                                state :keymap))
          local-mode (evil-state-property state :local-mode)
          local-keymap (symbol-value (evil-state-property
                                      state :local-keymap))
          tag (symbol-value (evil-state-property
                             state :tag)))
    (ert-info ("Update `evil-state'")
      (should (eq evil-state state)))
    (ert-info ("Ensure `evil-local-mode' is enabled")
      (evil-test-local-mode-enabled))
    (ert-info ("Enable state modes")
      (should (symbol-value mode))
      (should (symbol-value local-mode)))
    (ert-info ("Push state keymaps to the top")
      (evil-test-state-keymaps state))
    (ert-info ("Refresh modeline tag")
      (should (equal evil-modeline-tag tag)))))

(defun evil-test-state-keymaps (state)
  "Verify that STATE's keymaps are pushed to the top"
  (let ((actual (evil-state-keymaps state))
        (expected (list (symbol-value (evil-state-property
                                       state :local-keymap))
                        (symbol-value (evil-state-property
                                       state :keymap)))))
    ;; additional keymaps inherited with :enable
    (cond
     ((eq state 'operator)
      (setq expected
            (append expected
                    (list evil-normal-state-local-map
                          evil-normal-state-map
                          evil-operator-shortcut-map)))))
    (dotimes (i (length expected))
      (should (keymapp (nth i expected)))
      (should (eq (nth i actual) (nth i expected)))
      (should (memq (nth i expected) (current-active-maps)))
      (should (eq (cdr (nth i evil-mode-map-alist))
                  (nth i expected))))))

(ert-deftest evil-test-exit-normal-state ()
  "Enter Normal state and then disable all states"
  :tags '(evil)
  (with-temp-buffer
    (evil-test-change-state 'normal)
    (evil-normal-state -1)
    (evil-test-no-states)))

(ert-deftest evil-test-change-states ()
  "Change between Normal state, Emacs state and Operator-Pending state"
  :tags '(evil)
  (with-temp-buffer
    (evil-test-change-state 'normal)
    (evil-test-change-state 'emacs)
    (evil-test-change-state 'normal)
    (evil-test-change-state 'operator)
    (evil-test-change-state 'normal)
    (evil-test-change-state 'emacs)))

(ert-deftest evil-test-enter-normal-state-disabled ()
  "Enter Normal state even if `evil-local-mode' is disabled"
  :tags '(evil)
  (with-temp-buffer
    (evil-local-mode -1)
    (evil-test-local-mode-disabled)
    (evil-test-change-state 'normal)))

(defun evil-test-suppress-keymap (state)
  "Verify that `self-insert-command' is suppressed in STATE"
  (evil-test-buffer
    (evil-test-change-state state)
    (should (eq (key-binding "a") 'undefined))
    (should (eq (key-binding "b") 'undefined))
    (should (eq (key-binding "c") 'undefined))
    (ert-info ("Don't insert text")
      ;; may or may not signal an error, depending on batch mode
      (condition-case nil
          (execute-kbd-macro "abc")
        (error nil))
      (should (string= (buffer-substring 1 4) ";; ")))))

(ert-deftest evil-test-emacs-state-suppress-keymap ()
  "`self-insert-command' works in emacs-state"
  :tags '(evil)
  (should-error (evil-test-suppress-keymap 'emacs)))

(ert-deftest evil-test-normal-state-suppress-keymap ()
  "No `self-insert-command' in normal-state"
  :tags '(evil)
  (evil-test-suppress-keymap 'normal))

(ert-deftest evil-test-operator-state-suppress-keymap ()
  "Operator-Pending state should inherit suppression
of `self-insert-command' from Normal state"
  :tags '(evil)
  (evil-test-suppress-keymap 'operator))

(ert-deftest evil-test-operator-state-shortcut-keymap ()
  "Enable shortcut keymap in Operator-Pending state"
  :tags '(evil)
  (evil-test-buffer
    (ert-info ("Activate `evil-operator-shortcut-map' in
Operator-Pending state")
      (evil-test-change-state 'operator)
      (should (memq evil-operator-shortcut-map
                    (evil-state-keymaps 'operator)))
      (should (keymapp evil-operator-shortcut-map))
      (should evil-operator-shortcut-mode))
    (should (memq evil-operator-shortcut-map
                  (current-active-maps)))
    (ert-info ("Deactivate `evil-operator-shortcut-map'
outside Operator-Pending state")
      (evil-test-change-state 'emacs)
      (should-not evil-operator-shortcut-mode)
      (should-not (memq evil-operator-shortcut-map
                        (current-active-maps))))
    (ert-info ("Reset `evil-operator-shortcut-map'
when entering Operator-Pending state")
      (define-key evil-operator-shortcut-map "f" 'foo)
      (should (eq (lookup-key evil-operator-shortcut-map "f")
                  'foo))
      (evil-test-change-state 'operator)
      (should-not (eq (lookup-key evil-operator-shortcut-map "f")
                      'foo)))
    (ert-info ("Reset `evil-operator-shortcut-map'
when exiting Operator-Pending state")
      (define-key evil-operator-shortcut-map "b" 'bar)
      (should (eq (lookup-key evil-operator-shortcut-map "b")
                  'bar))
      (evil-test-change-state 'emacs)
      (should-not (eq (lookup-key evil-operator-shortcut-map "b")
                      'bar)))))

;;; Type system

(ert-deftest evil-test-exclusive-type ()
  "Expand and contract the `line' type"
  :tags '(evil)
  (evil-test-buffer
    (let* ((first-line 1)
           (second-line (progn
                          (forward-line)
                          (point)))
           (third-line (progn
                         (forward-line)
                         (point))))
      (ert-info ("Return the beginning and end unchanged
if they are the same")
        (should (equal (evil-expand 1 1 'exclusive)
                       (list 1 1 'exclusive))))
      (ert-info ("expand to `inclusive' if the end position
is at the beginning of a line")
        (should (equal (evil-expand (1+ first-line) second-line 'exclusive)
                       (list (1+ first-line) second-line 'inclusive))))
      (ert-info ("expand to `line' if both the beginning and end
are at the beginning of a line")
        (should (equal (evil-expand first-line second-line 'exclusive)
                       (list first-line second-line 'line))))
      (ert-info ("Measure as the strict difference between the end
and the beginning")
        (should (string= (evil-describe 1 1 'exclusive)
                         "0 characters"))
        (should (string= (evil-describe 1 2 'exclusive)
                         "1 character"))
        (should (string= (evil-describe 5 2 'exclusive)
                         "3 characters"))))))

(ert-deftest evil-test-inclusive-type ()
  "Expand and contract the `inclusive' type"
  :tags '(evil)
  (evil-test-buffer
    (ert-info ("Include the ending character")
      (should (equal (evil-expand 1 1 'inclusive)
                     '(1 2 inclusive))))
    (ert-info ("Don't mind if positions are in wrong order")
      (should (equal (evil-expand 5 2 'inclusive)
                     '(2 6 inclusive))))
    (ert-info ("Exclude the ending character when contracting")
      (should (equal (evil-contract 1 2 'inclusive)
                     '(1 1 inclusive))))
    (ert-info ("Don't mind positions order when contracting")
      (should (equal (evil-contract 6 2 'inclusive)
                     '(2 5 inclusive))))
    (ert-info ("Measure as one more than the difference")
      (should (string= (evil-describe 1 1 'inclusive)
                       "1 character"))
      (should (string= (evil-describe 5 2 'inclusive)
                       "4 characters")))))

(ert-deftest evil-test-line-type ()
  "Expand the `line' type"
  :tags '(evil)
  (evil-test-buffer
    (let* ((first-line 1)
           (second-line (progn
                          (forward-line)
                          (point)))
           (third-line (progn
                         (forward-line)
                         (point))))
      (ert-info ("Expand to the whole first line")
        (should (equal (evil-expand first-line first-line 'line)
                       (list first-line second-line 'line)))
        (should (string= (evil-describe first-line first-line 'line)
                         "1 line")))
      (ert-info ("Expand to the two first lines")
        (should (equal (evil-expand first-line second-line 'line)
                       (list first-line third-line 'line)))
        (should (string= (evil-describe first-line second-line 'line)
                         "2 lines"))))))

(ert-deftest evil-test-block-type ()
  "Expand and contract the `block' type"
  :tags '(evil)
  (evil-test-buffer
    (let* ((first-line 1)
           (second-line (progn
                          (forward-line)
                          (point)))
           (third-line (progn
                         (forward-line)
                         (point))))
      (ert-info ("Expand to a 1x1 block")
        (should (equal (evil-expand 1 1 'block)
                       (list 1 2 'block)))
        (should (string= (evil-describe 1 1 'block)
                         "1 row and 1 column")))
      (ert-info ("Expand to a 2x1 block")
        (should (equal (evil-expand first-line second-line 'block)
                       (list first-line (1+ second-line) 'block)))
        (should (string= (evil-describe first-line second-line 'block)
                         "2 rows and 1 column")))
      (ert-info ("Expand to a 3x2 block")
        (should (equal (evil-expand first-line (1+ third-line) 'block)
                       (list first-line (1+ (1+ third-line)) 'block)))
        (should (string= (evil-describe first-line (1+ third-line) 'block)
                         "3 rows and 2 columns")))
      (ert-info ("Contract to a 0x0 rectangle")
        (should (equal (evil-contract 1 2 'block)
                       (list 1 1 'block))))
      (ert-info ("Contract to a 2x0 rectangle")
        (should (equal (evil-contract first-line (1+ second-line) 'block)
                       (list first-line second-line 'block))))
      (ert-info ("Contract to a 3x1 rectangle")
        (should (equal (evil-contract first-line (1+ (1+ third-line)) 'block)
                       (list first-line (1+ third-line) 'block)))))))

;;; Utilities

(ert-deftest evil-test-truncate-vector ()
  "Test `evil-truncate-vector'"
  :tags '(evil)
  (ert-info ("Positive numbers")
    (should (equal (evil-truncate-vector [a b c] 0) []))
    (should (equal (evil-truncate-vector [a b c] 1) [a]))
    (should (equal (evil-truncate-vector [a b c] 2) [a b]))
    (should (equal (evil-truncate-vector [a b c] 3) [a b c]))
    (should (equal (evil-truncate-vector [a b c] 4) [a b c])))
  (ert-info ("Negative numbers")
    (should (equal (evil-truncate-vector [a b c] -1) [a b]))
    (should (equal (evil-truncate-vector [a b c] -2) [a]))
    (should (equal (evil-truncate-vector [a b c] -3) []))
    (should (equal (evil-truncate-vector [a b c] -4) [])))
  (ert-info ("Limit cases")
    (should (equal (evil-truncate-vector [] 0) []))
    (should (equal (evil-truncate-vector [] 3) []))))

(when evil-tests-run
  (evil-tests-run))

(provide 'evil-tests)

;;; evil-tests.el ends here
