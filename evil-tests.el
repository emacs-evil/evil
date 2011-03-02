;; evil-tests.el --- unit tests for Evil -*- coding: utf-8 -*-

;; This file is for developers. It runs some unit tests on Evil.
;; To load it, add the following line to .emacs:
;;
;;     (require 'evil-tests)
;;
;; This file is NOT part of Evil itself.

(require 'ert)
(require 'evil)

(defvar evil-tests-run t
  "Run Evil tests.")

(defmacro evil-test-buffer (&rest body)
  "Execute BODY in a temporary buffer.
The buffer contains the familiar *scratch* message."
  (declare (indent defun)
           (debug t))
  `(let ((kill-ring kill-ring)
         (kill-ring-yank-pointer kill-ring-yank-pointer)
         x-select-enable-clipboard
         message-log-max)
     (with-temp-buffer
       (save-window-excursion
         (switch-to-buffer-other-window (current-buffer))
         (buffer-enable-undo)
         (save-excursion
           (insert ";; This buffer is for notes you don't want to save, \
and for Lisp evaluation.\n;; If you want to create a file, visit \
that file with C-x C-f,\n;; then enter the text in that file's own \
buffer.\n"))
         ,@body))))

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
    (should (assq 'evil-vi-state-local-map (buffer-local-variables)))
    (should (keymapp evil-vi-state-local-map))
    (should (assq 'evil-emacs-state-local-map (buffer-local-variables)))
    (should (keymapp evil-emacs-state-local-map)))
  (ert-info ("Refresh buffer-local entries in `evil-mode-map-alist'")
    (should (rassq evil-vi-state-local-map evil-mode-map-alist))
    (should (rassq evil-emacs-state-local-map evil-mode-map-alist)))
  (ert-info ("Don't add buffer-local entries to the default value")
    (should-not (rassq evil-vi-state-local-map
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
      (should (equal (nth 0 evil-mode-map-alist)
                     (cons local-mode local-keymap)))
      (should (equal (nth 1 evil-mode-map-alist)
                     (cons mode keymap))))
    (ert-info ("Refresh modeline tag")
      (should (equal evil-modeline-tag tag)))))

(ert-deftest evil-test-exit-vi-state ()
  "Enter vi state and then disable all states"
  :tags '(evil)
  (with-temp-buffer
    (evil-test-change-state 'vi)
    (evil-vi-state -1)
    (evil-test-no-states)))

(ert-deftest evil-test-change-states ()
  "Change between vi state and emacs state"
  :tags '(evil)
  (with-temp-buffer
    (evil-test-change-state 'vi)
    (evil-test-change-state 'emacs)
    (evil-test-change-state 'vi)
    (evil-test-change-state 'emacs)))

(ert-deftest evil-test-enter-vi-state-disabled ()
  "Enter vi state even if `evil-local-mode' is disabled"
  :tags '(evil)
  (with-temp-buffer
    (evil-local-mode -1)
    (evil-test-local-mode-disabled)
    (evil-test-change-state 'vi)))

(ert-deftest evil-test-emacs-state-suppress-keymap ()
  "`self-insert-command' works in emacs-state"
  :tags '(evil)
  (evil-test-buffer
    (evil-local-mode 1)
    (evil-emacs-state 1)
    (goto-char (point-min))
    (execute-kbd-macro "abc")
    (should (string= "abc" (buffer-substring (point-min) (+ 3 (point-min)))))))

(ert-deftest evil-test-vi-state-suppress-keymap ()
  "No `self-insert-command' in vi-state"
  :tags '(evil)
  (evil-test-buffer
    (evil-local-mode 1)
    (evil-vi-state 1)
    (goto-char (point-min))
    (should-error (execute-kbd-macro "abc"))
    (should (string= ";; " (buffer-substring (point-min) (+ 3 (point-min)))))))

(when evil-tests-run
  (ert-run-tests-batch '(tag evil)))

(provide 'evil-tests)

;;; evil-tests.el ends here
