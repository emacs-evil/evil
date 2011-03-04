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

(defun evil-tests-run ()
  "Run Evil tests."
  (interactive)
  (ert-run-tests-batch '(tag evil)))

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
      (should (equal (nth 0 evil-mode-map-alist)
                     (cons local-mode local-keymap)))
      (should (equal (nth 1 evil-mode-map-alist)
                     (cons mode keymap))))
    (ert-info ("Refresh modeline tag")
      (should (equal evil-modeline-tag tag)))))

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

(defun evil-test-repeat-info (keys &optional recorded)
  "Executes a sequence of keys and verifies that `evil-repeat-info' records them correctly.
`keys' is the sequence of keys to execute
`recorded' is the expected sequence of recorded events, if nil `keys' is used"
  (execute-kbd-macro keys)
  (should (equal (vconcat evil-repeat-info)
                 (vconcat (or recorded keys)))))

(ert-deftest evil-test-normal-repeat-info-simple-command ()
  "Save key-sequence after simple editing command in vi-state"
  :tags '(evil)
  (evil-test-buffer
    (evil-test-change-state 'normal)
    (ert-info ("Call simple command without count")
      (evil-test-repeat-info "x"))
    (ert-info ("Call simple command with count 3")
      (evil-test-repeat-info "3x"))))

(ert-deftest evil-test-normal-repeat-info-char-command ()
  "Save key-sequence after editing command with character in vi-state"
  :tags '(evil)
  (evil-test-buffer
    (evil-test-change-state 'normal)
    (ert-info ("Call command with character argument without count")
      (evil-test-repeat-info "r5"))
    (ert-info ("Call command with character argument with count 12")
      (evil-test-repeat-info "12rX"))))

(ert-deftest evil-test-insert-repeat-info ()
  "Save key-sequence after insertion mode"
  :tags '(evil)
  (evil-test-buffer
    (evil-test-change-state 'normal)
    (ert-info ("Insert text without count")
      (evil-test-repeat-info (vconcat "iABC" [escape])))
    (ert-info ("Insert text with count 42")
      (evil-test-repeat-info (vconcat "42iABC" [escape])))))

(defun evil-test-editing (keys expected &optional point-char)
  "Execute key-sequence `keys' and verify if the text around point matches
`expected' afterwards.
`keys' is a sequence of events to be passed to `execute-kbd-macro'
`expected' is a regexp with a special character marking (point)'s position
`point-char' is the special character marking (point)'s position, defaulted to °
If, e.g., expected is \"ABC°def\" this means the expected text before point is
\"ABC\" and the expected text after point is \"def\". "
  (setq point-char (regexp-quote (char-to-string (or point-char ?°))))
  (string-match point-char expected)
  (unless (match-beginning 0)
    (error "No cursor specified in expected string: %s" expected))
  (let ((before (substring expected 0 (match-beginning 0)))
        (after (substring expected (match-end 0))))
    (execute-kbd-macro keys)
    (ert-info ((format "Text before point is %s"
                       (buffer-substring (max (point-min)
                                              (- (point) (length before)))
                                         (point))))
      (should (looking-back before)))
    (ert-info ((format "Text after point is %s"
                       (buffer-substring (point)
                                         (min (point-max)
                                              (+ (point) (length after))))))
      (should (looking-at after)))))

(defun evil-test-editing-clean (keys expected &optional point-char)
  "The same as `evil-test-editing' but starts with a new
unchanged test-buffer in normal-state."
  (evil-test-buffer
    (evil-test-change-state 'normal)
    (evil-test-editing keys expected point-char)))


(ert-deftest evil-test-repeat ()
  "Repeat several editing commands."
  :tags '(evil)
  (ert-info ("Repeat insert")
    (evil-test-editing-clean (vconcat "iABC" [escape] "..") "ABABAB°CCC;; This"))

  (ert-info ("Repeat replace")
    (evil-test-editing-clean (vconcat "rX" [right right] ".") "\\`X;°XThis"))

  (ert-info ("Repeat replace with count")
    (evil-test-editing-clean (vconcat "2rX" [right right] ".") "\\`XX X°Xis ")))

(ert-deftest evil-test-cmd-replace-char ()
  "Calling `evil-replace-char' should replace characters."
  :tags '(evil)
  (evil-test-editing-clean "r5" "\\`°5; This")
  (evil-test-editing-clean "3rX" "\\`XX°XThis"))

(ert-deftest evil-test-insert-before ()
  "Test insertion of text before point"
  :tags '(evil)
  (evil-test-buffer
    (evil-local-mode 1)
    (goto-char (+ 3 (point-min)))
    (should (and (looking-at "This") (looking-back ";; ")))
    (evil-test-editing  (vconcat "ievil rulz " [escape])
                        "\\`;; evil rulz° This")))


(when evil-tests-run
  (evil-tests-run))

(provide 'evil-tests)

;;; evil-tests.el ends here
