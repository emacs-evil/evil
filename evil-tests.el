;; evil-tests.el --- unit tests for Evil -*- coding: utf-8 -*-

;; Author: Vegard Øye <vegard_oye at hotmail.com>
;; Maintainer: Vegard Øye <vegard_oye at hotmail.com>

;; Version: 1.2.13

;;
;; This file is NOT part of GNU Emacs.

;;; License:

;; This file is part of Evil.
;;
;; Evil is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Evil is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Evil.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

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
(require 'evil-test-helpers)

;;; Code:

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
        (expected `((,(evil-state-property state :local)
                     . , (evil-state-property state :local-keymap t))
                    (,(evil-state-property state :mode)
                     . ,(evil-state-property state :keymap t)))))
    ;; additional keymaps inherited with :enable
    (cond
     ((eq state 'operator)
      (setq expected
            `((evil-operator-shortcut-mode
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
        (should (keymapp map))))))

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

(ert-deftest evil-test-change-to-previous-state ()
  "Change to some state and back."
  :tags '(evil state)
  (with-temp-buffer
    (evil-test-change-state 'normal)
    (evil-test-change-state 'visual)
    (evil-test-change-state 'emacs)
    (evil-change-to-previous-state)
    (should (eq evil-state 'visual))
    (evil-change-to-previous-state)
    (should (eq evil-state 'normal))))

(ert-deftest evil-test-enter-normal-state-disabled ()
  "Enter Normal state even if `evil-local-mode' is disabled"
  :tags '(evil state)
  (with-temp-buffer
    (evil-local-mode -1)
    (evil-test-local-mode-disabled)
    (evil-test-change-state 'normal)))

(ert-deftest evil-test-execute-in-normal-state ()
  "Test `evil-execute-in-normal-state'."
  :tags '(evil)
  (ert-info ("Execute normal state command in insert state")
    (evil-test-buffer
      "[a]bcdef\n"
      ("I")
      (should (evil-insert-state-p))
      ("\C-ox")
      (ert-info ("Should return to insert state")
        (should (evil-insert-state-p)))
      "[b]cdef\n"
      ("\C-oA")
      (ert-info ("Should return to insert state after insert state command")
        (should (evil-insert-state-p)))
      ("bcdef[]\n"))))

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

(ert-deftest evil-test-global-local-map-binding ()
  "Test use of `evil-define-key' for binding in global maps."
  :tags '(evil state)
  (let ((evil-normal-state-map (copy-keymap evil-normal-state-map))
        (evil-normal-state-local-map
         (when (keymapp evil-normal-state-local-map)
           (copy-keymap evil-normal-state-local-map)))
        (global-map (copy-keymap global-map))
        (orig-local-map
         (when (keymapp (current-local-map))
           (copy-keymap (current-local-map))))
        (map (or (current-local-map) (make-sparse-keymap))))
    (use-local-map map)
    (ert-info ("Bind in a global state map")
      (evil-define-key 'normal 'global "f" 'foo)
      (should (eq (lookup-key evil-normal-state-map "f") 'foo)))
    (ert-info ("Bind in a local state map")
      (evil-define-key 'normal 'local "f" 'foo)
      (should (eq (lookup-key evil-normal-state-local-map "f") 'foo)))
    (ert-info ("Bind in the global map")
      (evil-define-key nil 'global "b" 'bar)
      (should (eq (lookup-key global-map "b") 'bar)))
    (ert-info ("Bind in the local map")
      (evil-define-key nil 'local "b" 'bar)
      (should (eq (lookup-key (current-local-map) "b") 'bar)))
    (use-local-map orig-local-map)))

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
      (should (equal (evil-transform :expand 1 2 nil)
                     '(1 2)))
      (should (equal (evil-transform nil 1 2 nil)
                     '(1 2))))
    (ert-info ("Accept markers, but return positions")
      (should (equal (evil-transform :expand
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

(ert-deftest evil-test-visual-append ()
  "Test `evil-append' from visual state"
  :tags '(evil insert)
  (evil-test-buffer
   ";; [T]his buffer is for notes you don't want to save"
   ("veA_evil rulz " [escape])
   ";; This_evil rulz[ ] buffer is for notes you don't want to save"))

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
;; and for Lisp evaluation.")
  (ert-info ("Open empty line")
    (evil-test-buffer
      "(let (var)\n  [t]est)\n"
      (emacs-lisp-mode)
      ("O" [escape])
      "(let (var)\n[\n]  test)\n"))
  (ert-info ("Open non-empty line")
    (evil-test-buffer
      "(let (var)\n  [t]est)\n"
      (emacs-lisp-mode)
      ("Odo-it" [escape])
      "(let (var)\n  do-i[t]\n  test)\n")))

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
;; and for Lisp evaluation.")
  (ert-info ("Open empty line")
    (evil-test-buffer
      "[(]let (var)\n  test)\n"
      (emacs-lisp-mode)
      ("o" [escape])
      "(let (var)\n[\n]  test)\n"))
  (ert-info ("Open non-empty line")
    (evil-test-buffer
      "[(]let (var)\n  test)\n"
      (emacs-lisp-mode)
      ("odo-it" [escape])
      "(let (var)\n  do-i[t]\n  test)\n"))
  (let ((evil-auto-indent t))
    (ert-info ("With count")
      (evil-test-buffer
        "[(]and a\n     c)\n"
        (emacs-lisp-mode)
        ("3ob" [escape])
        "(and a\n     b\n     b\n     [b]\n     c)\n"))))

(ert-deftest evil-test-open-below-folded ()
  "Test `evil-open-below' on folded lines"
  :tags '(evil insert)
  (evil-test-buffer
    "[l]ine1\n\n(let ()\n  var)\n\nlast line\n"
    (emacs-lisp-mode)
    (hs-minor-mode 1)
    ("zm2joABC" [escape])
    "line1\n\n(let ()\n  var)\nAB[C]\n\nlast line\n"))

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

(ert-deftest evil-test-repeat-register ()
  "Test repeating a register command."
  :tags '(evil repeat)
  (evil-test-buffer
    "[l]ine 1\nline 2\nline 3\nline 4\n"
    ("\"addyy\"aP")
    "[l]ine 1\nline 2\nline 3\nline 4\n"
    (".")
    "[l]ine 1\nline 1\nline 2\nline 3\nline 4\n"))

(ert-deftest evil-test-repeat-numeric-register ()
  "Test repeating a command with a numeric register."
  :tags '(evil repeat)
  (evil-test-buffer
    "[l]ine 1\nline 2\nline 3\nline 4\nline 5\n"
    ("dd...")
    "[l]ine 5\n"
    ("\"1P")
    "[l]ine 4\nline 5\n"
    (".")
    "[l]ine 3\nline 4\nline 5\n"
    (".")
    "[l]ine 2\nline 3\nline 4\nline 5\n"
    (".")
    "[l]ine 1\nline 2\nline 3\nline 4\nline 5\n"))

(ert-deftest evil-test-cmd-replace-char ()
  "Calling `evil-replace-char' should replace characters"
  :tags '(evil repeat)
  (evil-test-buffer
    "[;]; This buffer is for notes you don't want to save"
    ("r5")
    "[5]; This buffer is for notes you don't want to save"
    ("3rX")
    "XX[X]This buffer is for notes you don't want to save")
  (ert-info ("Replace digraph")
    (evil-test-buffer
      "[;]; This buffer is for notes you don't want to save"
      ("re'")
      "[é]; This buffer is for notes you don't want to save"
      ("3rc*")
      "ξξ[ξ]This buffer is for notes you don't want to save"))
  (ert-info ("Replacing \\n should insert only one newline")
    (evil-test-buffer
      "(setq var xxx [y]yy zzz)\n"
      (emacs-lisp-mode)
      (setq indent-tabs-mode nil)
      ("2r\n")
      "(setq var xxx \n      [y] zzz)\n")))

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

(ert-deftest evil-test-repeat-error ()
  "Test whether repeat returns to normal state in case of an error."
  (evil-test-buffer
    "[l]ine 1\nline 2\nline 3\nline 4"
    ("ixxx" [down] [down] [left] [left] [left] "yyy" [escape])
    "xxxline 1\nline 2\nyy[y]line 3\nline 4"
    (should-error (execute-kbd-macro "j^."))
    (should (evil-normal-state-p))
    ("^")
    "xxxline 1\nline 2\nyyyline 3\n[x]xxline 4"))

(ert-deftest evil-test-repeat-quoted-insert ()
  "Test whether `quoted-insert' can be repeated."
  (ert-info ("Insert C-v")
    (evil-test-buffer
      "lin[e] 1\nline 2\nline 3\n"
      ("i\C-v\C-v" [escape])
      "lin[]e 1\nline 2\nline 3\n"))
  (ert-info ("Insert ESC")
    (evil-test-buffer
      "lin[e] 1\nline 2\nline 3\n"
      ("i\C-v" [escape escape])
      "lin[]e 1\nline 2\nline 3\n"))
  (ert-info ("Block insert C-v")
    (evil-test-buffer
      "lin[e] 1\nline 2\nline 3\n"
      ("gg\C-vGI\C-v\C-v" [escape])
      "[]line 1\nline 2\nline 3\n")))

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

(ert-deftest evil-visual-block-append ()
  "Test appending in visual block."
  :tags '(evil visual insert)
  (ert-info ("Simple append")
    (evil-test-buffer
      "l[i]ne 1\nline 2\nline 3\n"
      ((kbd "C-v") "jjllAXXX" [escape])
      "lineXX[X] 1\nlineXXX 2\nlineXXX 3\n"))
  (ert-info ("Append after empty lines")
    (evil-test-buffer
      "line 1l[i]ne 1\nline 2\nline 3line 3\n"
      (setq indent-tabs-mode nil)
      ((kbd "C-v") "jjllAXXX" [escape])
      "line 1lineXX[X] 1\nline 2    XXX\nline 3lineXXX 3\n"))
  (ert-info ("Append after empty first line")
    (evil-test-buffer
      "l[i]ne 1line 1\nline 2\nline 3line 3line 3\n"
      (setq indent-tabs-mode nil)
      ((kbd "C-v") "jj3feAXXX" [escape])
      "line 1line 1    XX[X]\nline 2          XXX\nline 3line 3lineXXX 3\n"))
  (ert-info ("Append after end of lines")
    (evil-test-buffer
      "line 1l[i]ne 1line 1\nline 2\nline 3line 3\n"
      (setq indent-tabs-mode nil)
      ((kbd "C-v") "jj$AXXX" [escape])
      "line 1line 1line 1XX[X]\nline 2XXX\nline 3line 3XXX\n")))

(ert-deftest evil-test-repeat-digraph ()
  "Test repeat of insertion of a digraph."
  :tags '(evil digraph repeat)
  (evil-test-buffer
    "Line with ['] several apostrophes ', yeah."
    ("s" (kbd "C-k") "'9" [escape])
    "Line with [’] several apostrophes ', yeah."
    ("f'.")
    "Line with ’ several apostrophes [’], yeah."))

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
               '(evil-digit-argument-or-evil-beginning-of-line nil))))
    (ert-info ("Handle keyboard macros")
      (evil-test-buffer
        (define-key evil-motion-state-local-map (kbd "W") (kbd "w"))
        (should (equal (evil-keypress-parser '(?W))
                       '(evil-forward-word-begin nil)))))))

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
  :tags '(evil operator yank)
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
                  'evil-yank-block-handler))))
  (ert-info (":yank, then paste")
    (evil-test-buffer
     "a\n[b]\nc\nd\n"
     (":yank" [return] "p")
     "a\nb\nb\nc\nd\n"))
  (ert-info (":yank with COUNT")
    (evil-test-buffer
     "a\n[b]\nc\nd\n"
     (":yank 2" [return] "p")
     "a\nb\nb\nc\nc\nd\n"))
  (ert-info (":yank with COUNT in visual state")
    (evil-test-buffer
     "a\n<b\nc>\nd\ne\nf\n"
     (":yank 3" [return] "p")
     "a\nb\nc\nd\ne\nc\nd\ne\nf\n"))
  (ert-info (":yank with REGISTER")
    (evil-test-buffer
     "a\n[b]\nc\nd\n"
     (":yank r") ;; yank into the 'r' register
     "a\nb\nc\nd\n"
     ;; check the 'r' register contains the yanked text
     (should (string= (substring-no-properties (evil-get-register ?r)) "b\n"))))
  (ert-info (":yank with REGISTER and COUNT")
    (evil-test-buffer
     "a\n[b]\nc\nd\ne\nf\n"
     (":yank r 3")
     "a\nb\nc\nd\ne\nf\n"
     (should (string= (substring-no-properties (evil-get-register ?r)) "b\nc\nd\n")))))

(ert-deftest evil-test-delete ()
  "Test `evil-delete'"
  :tags '(evil operator delete)
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
  (ert-info ("Delete last empty line")
    (evil-test-buffer
      "line 1\nline 2\n\n[]"
      ("dd")
      "line 1\nline 2\n[]"))
  (ert-info ("Delete rectangle")
    (evil-test-buffer
      "[;]; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      (define-key evil-operator-state-local-map "s" 'evil-test-square-motion)
      ("d3s")
      "[T]his buffer is for notes you don't want to save.
If you want to create a file, visit that file with C-x C-f,
then enter the text in that file's own buffer."))
  (ert-info (":delete")
    (evil-test-buffer
     "a\n[b]\nc\nd\n"
     (":delete")
     "a\nc\nd\n"))
  (ert-info (":delete with COUNT")
    (evil-test-buffer
     "a\n[b]\nc\nd\n"
     (":delete 2")
     "a\nd\n"))
  (ert-info (":delete with COUNT in visual state")
    (evil-test-buffer
     "a\n<b\nc>\nd\ne\nf\n"
     (":delete 3")
     "a\nb\nf\n"))
  (ert-info (":delete with REGISTER")
    (evil-test-buffer
     "a\n[b]\nc\nd\n"
     (":delete r") ;; delete into the 'r' register
     "a\nc\nd\n"
     ;; check the 'r' register contains the deleted text
     (should (string= (substring-no-properties (evil-get-register ?r)) "b\n"))))
  (ert-info (":delete with REGISTER and COUNT")
    (evil-test-buffer
     "a\n[b]\nc\nd\ne\nf\n"
     (":delete r 3")
     "a\ne\nf\n"
     (should (string= (substring-no-properties (evil-get-register ?r)) "b\nc\nd\n")))))

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
;; and for Lisp evalua"))
  (ert-info ("Yank full block with block selection")
    (evil-test-buffer
      :visual block
      "line1 l<ine1 line1 line1\nline2 line2\nline3 lin>e3 line3\n"
      ("D")
      "line1 [l]\nline2 l\nline3 l\n"
      ("0P")
      "ine1 line1 line1line1 l
ine2            line2 l
ine3 line3      line3 l\n")))

(ert-deftest evil-test-delete-folded ()
  "Test `evil-delete' on folded lines."
  :tags '(evil operator)
  (ert-info ("Delete folded lines")
    (evil-test-buffer
      "[l]ine1\n\n(let ()\n  var)\n\n(let ()\n  var2)\n"
      (emacs-lisp-mode)
      (hs-minor-mode 1)
      ("zm2jdd")
      "line1\n\n[\n](let ()\n  var2)\n"))
  (ert-info ("Delete folded lines with count")
    (evil-test-buffer
      "[l]ine1\n\n(let ()\n  var)\n\n(let ()\n  var2)\n\nlast line\n"
      (emacs-lisp-mode)
      (hs-minor-mode 1)
      ("zm2j3dd")
      "line1\n\n[\n]last line\n")))

(ert-deftest evil-test-delete-backward-word ()
  "Test `evil-delete-backward-word' in insert state."
  :tags '(evil)
  (let ((evil-backspace-join-lines t))
    (evil-test-buffer
      "abc def\n   ghi j[k]l\n"
      ("i" (kbd "C-w"))
      "abc def\n   ghi [k]l\n"
      ((kbd "C-w"))
      "abc def\n   [k]l\n"
      ((kbd "C-w"))
      "abc def\n[k]l\n"
      ((kbd "C-w"))
      "abc def[k]l\n"))
  (let (evil-backspace-join-lines)
    (evil-test-buffer
      "abc def\n[k]l\n"
      (should-error (execute-kbd-macro (concat "i" (kbd "C-w"))))
      "abc def\n[k]l\n")))

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
      "AB[C] This buffer is for notes."))
  (ert-info ("Whitespace")
    (evil-test-buffer
      "This[ ]is a test\n"
      ("cwABC" [escape])
      "ThisAB[C]is a test\n")))

(ert-deftest evil-test-join ()
  "Test `evil-join'"
  :tags '(evil join operator)
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
;; If you want to create a file, visit that file with C-x C-f."))
  (ert-info ("Join with count")
    (evil-test-buffer
     "[l]ine 1\nline 2\nline 3\nline 4"
     (":join 3")
     "line 1 line 2 line 3\nline 4"))
  (ert-info ("Join with bang and count")
    (evil-test-buffer
     "[l]ine 1\nline 2\nline 3\nline 4"
     (":join! 3")
     "line 1line 2line 3\nline 4"))
  (ert-info ("Join with bang and count, exceeding end-of-buffer")
    (evil-test-buffer
     "[l]ine 1\nline 2\nline 3\nline 4"
     (":join! 10")
     "line 1line 2line 3line 4"))
  (ert-info ("Join with count 1 should be the same as without count")
    (evil-test-buffer
     "[l]ine 1\nline 2\nline 3\nline 4"
     (":join 1")
     "line 1 line 2\nline 3\nline 4"))
  (ert-info ("Join with count 2 should be the same as with count 1")
    (evil-test-buffer
     "[l]ine 1\nline 2\nline 3\nline 4"
     (":join 2")
     "line 1 line 2\nline 3\nline 4"))
  (ert-info ("Join with count and single line range")
    (evil-test-buffer
     "[l]ine 1\nline 2\nline 3\nline 4"
     (":2join 3")
     "line 1\nline 2 line 3 line 4"))
  (ert-info ("Join with count and range")
    (evil-test-buffer
     "[l]ine 1\nline 2\nline 3\nline 4"
     (":1,2join 3")
     "line 1\nline 2 line 3 line 4"))
  (ert-info ("Join with count, range and bang")
    (evil-test-buffer
     "[l]ine 1\nline 2\nline 3\nline 4"
     (":1,2join! 3")
     "line 1\nline 2line 3line 4"))
  (ert-info ("Join with range")
    (evil-test-buffer
     "[l]ine 1\nline 2\nline 3\nline 4"
     (":1,3join")
     "line 1 line 2 line 3\nline 4"))
  )

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

(ert-deftest evil-test-shift ()
  "Test `evil-shift-right' and `evil-shift-left'."
  :tags '(evil operator)
  (let ((evil-shift-width 4)
        indent-tabs-mode)
    (ert-info ("Shift linewise")
      (evil-test-buffer
        "[l]ine 1\nline 2\nline 3\n"
        ("Vj>")
        "    [l]ine 1\n    line 2\nline 3\n"))
    (ert-info ("Shift char selection on whole line")
      (evil-test-buffer
        "[l]ine 1\nline 2\nline 3\n"
        ("v$>")
        "    [l]ine 1\nline 2\nline 3\n"))
    (ert-info ("Shift visual with count")
      (evil-test-buffer
        "[l]ine 1\nline 2\nline 3\n"
        ("Vj3>")
        "            [l]ine 1\n            line 2\nline 3\n"
        ("Vj2<")
        "    [l]ine 1\n    line 2\nline 3\n"))
    (ert-info ("Shift in insert state")
      (evil-test-buffer
        "line 1\nl[i]ne 2\nline 3\n"
        ("i\C-t\C-t")
        "line 1\n        l[i]ne 2\nline 3\n"
        ("\C-d")
        "line 1\n    l[i]ne 2\nline 3\n"))))

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
;; and for Lisp evaluation."
      ("2yyG$")
      ";; This buffer is for notes you don't want to save,
;; and for Lisp evaluation[.]"
      ("2P")
      ";; This buffer is for notes you don't want to save,
\[;]; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.
;; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.
;; and for Lisp evaluation."))
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
;; and for Lisp evaluation."
      ("2yyG$")
      ";; This buffer is for notes you don't want to save,
;; and for Lisp evaluation[.]"
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
;; then enter the text in that file's own buffer.  ;;"))
  (ert-info ("Paste preserves preceding text properties")
    (evil-test-buffer
     "[;]; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
     (put-text-property (point) (line-end-position) 'font-lock-face 'warning)
     ("yyp")
     ";; This buffer is for notes you don't want to save.
[;]; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
 (should (equal (get-text-property (point-min) 'font-lock-face) 'warning)))))

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

(ert-deftest evil-test-visual-paste-pop ()
  "Test `evil-paste-pop' after visual paste."
  :tags '(evil paste)
  (ert-info ("Visual-char paste, char paste")
    (evil-test-buffer
      "[w]ord1a word1b word1c\nword2a word2b\nword3a word3b word3c word3d\n"
      ("yiwyywyiw^jw")
      "word1a word1b word1c\nword2a [w]ord2b\nword3a word3b word3c word3d\n"
      ("viwp")
      "word1a word1b word1c\nword2a word1[b]\nword3a word3b word3c word3d\n"))
  (ert-info ("Visual-char paste, char paste, line pop")
    (evil-test-buffer
      "[w]ord1a word1b word1c\nword2a word2b\nword3a word3b word3c word3d\n"
      ("yiwyywyiw^jw")
      "word1a word1b word1c\nword2a [w]ord2b\nword3a word3b word3c word3d\n"
      ("viwp\C-p")
      "word1a word1b word1c\nword2a \n[w]ord1a word1b word1c\n\nword3a word3b word3c word3d\n"))
  (ert-info ("Visual-char paste, char paste, line pop, char pop")
    (evil-test-buffer
      "[w]ord1a word1b word1c\nword2a word2b\nword3a word3b word3c word3d\n"
      ("yiwyywyiw^jw")
      "word1a word1b word1c\nword2a [w]ord2b\nword3a word3b word3c word3d\n"
      ("viwp\C-p\C-p")
      "word1a word1b word1c\nword2a word1[a]\nword3a word3b word3c word3d\n"))
  (ert-info ("Visual-line paste, char paste")
    (evil-test-buffer
      "[w]ord1a word1b word1c\nword2a word2b\nword3a word3b word3c word3d\n"
      ("yiwyywyiw^j")
      "word1a word1b word1c\n[w]ord2a word2b\nword3a word3b word3c word3d\n"
      ("Vp")
      "word1a word1b word1c\nword1[b]word3a word3b word3c word3d\n"))
  (ert-info ("Visual-line paste, char paste, line pop")
    (evil-test-buffer
      "[w]ord1a word1b word1c\nword2a word2b\nword3a word3b word3c word3d\n"
      ("yiwyywyiw^j")
      "word1a word1b word1c\n[w]ord2a word2b\nword3a word3b word3c word3d\n"
      ("Vp\C-p")
      "word1a word1b word1c\n[w]ord1a word1b word1c\nword3a word3b word3c word3d\n"))
  (ert-info ("Visual-line paste, char paste, line pop, char pop")
    (evil-test-buffer
      "[w]ord1a word1b word1c\nword2a word2b\nword3a word3b word3c word3d\n"
      ("yiwyywyiw^j")
      "word1a word1b word1c\n[w]ord2a word2b\nword3a word3b word3c word3d\n"
      ("Vp\C-p\C-p")
      "word1a word1b word1c\nword1[a]word3a word3b word3c word3d\n")))

(ert-deftest evil-test-register ()
  "Test yanking and pasting to and from register."
  :tags '(evil yank paste)
  (ert-info ("simple lower case register")
    (evil-test-buffer
      "[f]oo\n"
      ("\"ayw\"aP")
      "fo[o]foo\n"
      ("\"ayy\"aP")
      "[f]oofoo\nfoofoo\n"))
  (ert-info ("upper case register")
    (evil-test-buffer
      "[f]oo\n"
      ("\"ayw\"Ayw\"aP")
      "foofo[o]foo\n"
      ("\"ayy\"Ayy\"aP")
      "[f]oofoofoo\nfoofoofoo\nfoofoofoo\n"))
  (ert-info ("upper case register and lines")
    (evil-test-buffer
      "[l]ine 1\nline 2\nline 3\nline 4\n"
      ("\"a2Yjj\"A2Y\"aP")
      "line 1\nline 2\n[l]ine 1\nline 2\nline 3\nline 4\nline 3\nline 4\n"
      ("8G\"ap")
      "line 1\nline 2\nline 1\nline 2\nline 3\nline 4\nline 3\nline 4\n[l]ine 1\nline 2\nline 3\nline 4\n"))
  (ert-info ("yank with count")
    (evil-test-buffer
      "[l]ine 1\nline 2\nline 3\n"
      ("\"a2yw\"aP")
      "line [1]line 1\nline 2\nline 3\n"
      ("\"a2yy\"aP")
      "[l]ine 1line 1\nline 2\nline 1line 1\nline 2\nline 3\n"))
  (dolist (module '(evil-search isearch))
    (evil-select-search-module 'evil-search-module module)
    (ert-info ((format "special register / (module: %s)" module))
      (evil-test-buffer
        "[f]oo bar\n"
        ("/bar" [return] "0i\C-r/")
        "bar[f]oo bar\n")))
  (ert-info ("special register :")
    (evil-test-buffer
      "[f]oo bar\n"
      (":noh\ni\C-r:"))))

(ert-deftest evil-test-last-insert-register ()
  "Test last insertion register."
  (evil-test-buffer
    "[l]ine 1\n"
    ("GiABC" [escape])
    "line 1\nAB[C]"
    ("gg\".P")
    "AB[C]line 1\nABC"))

(ert-deftest evil-test-zero-register ()
  "\"0 contains the last text that was yanked without specificying a register."
  (evil-test-buffer
    "[l]ine 1\nline 2\n"
    ("yy\"0p")
    "line 1\n[l]ine 1\nline 2\n"
    ("j\"ayy\"0p")
    "line 1\nline 1\nline 2\n[l]ine 1\n" ; yanked line 2 to "a, so "0 is still line 1
    ("kdd\"0p")
    "line 1\nline 1\nline 1\n[l]ine 1\n"))

(ert-deftest evil-test-align ()
  "Test `evil-align-left', `evil-align-right' and `evil-align-center'."
  :tags '(evil operator)
  (evil-without-display
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
        "before\n                 [l]ine 1\n         this is line number 2\n             line number 3\nafter\n"))))

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

(ert-deftest evil-test-preserve-column ()
  "Test `evil-previous-line' and `evil-next-line' preserve the column."
  :tags '(evil motion)
  (ert-info ("Simple")
    (evil-test-buffer
      "ab[c]\nabcdef\n\nabcd\n"
      ("j")
      "abc\nab[c]def\n\nabcd\n")
    (evil-test-buffer
      "ab[c]\nabcdef\n\nabcd\n"
      ("jj")
      "abc\nabcdef\n[\n]abcd\n")
    (evil-test-buffer
      "ab[c]\nabcdef\n\nabcd\n"
      ("jjj")
      "abc\nabcdef\n\nab[c]d\n")
    (evil-test-buffer
      "ab[c]\nabcdef\n\nabcd\n"
      ("jjjk")
      "abc\nabcdef\n[\n]abcd\n")
    (evil-test-buffer
      "ab[c]\nabcdef\n\nabcd\n"
      ("jjjkk")
      "abc\nab[c]def\n\nabcd\n")))

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

(ert-deftest evil-test-forward-not-word ()
  "Test `evil-forward-not-thing'"
  :tags '(evil motion)
  (evil-test-buffer
    "[ ]    aa,,"
    (evil-forward-not-thing 'evil-word)
    "     [a]a,,"))

;; TODO: test Visual motions and window motions
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
      "Some line with trailing whitespace  [ ]     \n    next line\n"
      ("dw")
      "Some line with trailing whitespace [ ]\n    next line\n")
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
      (should-error (execute-kbd-macro "10w"))))
  (ert-info ("Before last character in buffer")
    (evil-test-buffer
      "fo[o]."
      ("w")
      "foo[.]")
    (evil-test-buffer
      "fo[o] "
      ("w")
      "foo[ ]")
    (evil-test-buffer
      "[ ]e"
      ("w")
      " [e]")))

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

(ert-deftest evil-test-forward-word-begin-cjk ()
  "Test `evil-forward-word-begin' on CJK words"
  :tags '(evil motion cjk)
  (ert-info ("Latin / numeric")
    (evil-test-buffer
      "[a]bcd1234"
      ("w")
      "abcd123[4]"))
  (ert-info ("Latin / Kanji")
    (evil-test-buffer
      "[a]bcd漢字"
      ("w")
      "abcd[漢]字"))
  (ert-info ("Latin / Hiragana")
    (evil-test-buffer
      "[a]bcdひらがな"
      ("w")
      "abcd[ひ]らがな"))
  (ert-info ("Latin / Katakana")
    (evil-test-buffer
      "[a]bcdカタカナ"
      ("w")
      "abcd[カ]タカナ"))
  (ert-info ("Latin / half-width Katakana")
    (evil-test-buffer
      "[a]bcdｶﾀｶﾅ"
      ("w")
      "abcdｶﾀｶ[ﾅ]"))
  (ert-info ("Latin / full-width alphabet")
    (evil-test-buffer
      "[a]bcdＡＢＣ"
      ("w")
      "abcdＡＢ[Ｃ]"))
  (ert-info ("Latin / full-width numeric")
    (evil-test-buffer
      "[a]bcd１２３"
      ("w")
      "abcd１２[３]"))
  (ert-info ("Latin / Hangul")
    (evil-test-buffer
      "[a]bcd한글"
      ("w")
      "abcd[한]글"))
  (ert-info ("numeric / Latin")
    (evil-test-buffer
      "[1]234abcd"
      ("w")
      "1234abc[d]"))
  (ert-info ("numeric / Kanji")
    (evil-test-buffer
      "[1]234漢字"
      ("w")
      "1234[漢]字"))
  (ert-info ("numeric / Hiragana")
    (evil-test-buffer
      "[1]234ひらがな"
      ("w")
      "1234[ひ]らがな"))
  (ert-info ("numeric / Katakana")
    (evil-test-buffer
      "[1]234カタカナ"
      ("w")
      "1234[カ]タカナ"))
  (ert-info ("numeric / half-width Katakana")
    (evil-test-buffer
      "[1]234ｶﾀｶﾅ"
      ("w")
      "1234ｶﾀｶ[ﾅ]"))
  (ert-info ("numeric / full-width alphabet")
    (evil-test-buffer
      "[1]234ＡＢＣ"
      ("w")
      "1234ＡＢ[Ｃ]"))
  (ert-info ("numeric / full-width numeric")
    (evil-test-buffer
      "[1]234１２３"
      ("w")
      "1234１２[３]"))
  (ert-info ("numeric / Hangul")
    (evil-test-buffer
      "[1]234한글"
      ("w")
      "1234[한]글"))
  (ert-info ("Kanji / Latin")
    (evil-test-buffer
      "[漢]字abcd"
      ("w")
      "漢字[a]bcd"))
  (ert-info ("Kanji / numeric")
    (evil-test-buffer
      "[漢]字1234"
      ("w")
      "漢字[1]234"))
  (ert-info ("Kanji / Hiragana")
    (evil-test-buffer
      "[漢]字ひらがな"
      ("w")
      "漢字[ひ]らがな"))
  (ert-info ("Kanji / Katakana")
    (evil-test-buffer
      "[漢]字カタカナ"
      ("w")
      "漢字[カ]タカナ"))
  (ert-info ("Kanji / half-width Katakana")
    (evil-test-buffer
      "[漢]字ｶﾀｶﾅ"
      ("w")
      "漢字[ｶ]ﾀｶﾅ"))
  (ert-info ("Kanji / full-width alphabet")
    (evil-test-buffer
      "[漢]字ＡＢＣ"
      ("w")
      "漢字[Ａ]ＢＣ"))
  (ert-info ("Kanji / full-width numeric")
    (evil-test-buffer
      "[漢]字１２３"
      ("w")
      "漢字[１]２３"))
  (ert-info ("Kanji / Hangul")
    (evil-test-buffer
      "[漢]字한글"
      ("w")
      "漢字[한]글"))
  (ert-info ("Hiragana / Latin")
    (evil-test-buffer
      "[ひ]らがなabcd"
      ("w")
      "ひらがな[a]bcd"))
  (ert-info ("Hiragana / numeric")
    (evil-test-buffer
      "[ひ]らがな1234"
      ("w")
      "ひらがな[1]234"))
  (ert-info ("Hiragana / Kanji")
    (evil-test-buffer
      "[ひ]らがな漢字"
      ("w")
      "ひらがな[漢]字"))
  (ert-info ("Hiragana / Katakana")
    (evil-test-buffer
      "[ひ]らがなカタカナ"
      ("w")
      "ひらがな[カ]タカナ"))
  (ert-info ("Hiragana / half-width Katakana")
    (evil-test-buffer
      "[ひ]らがなｶﾀｶﾅ"
      ("w")
      "ひらがな[ｶ]ﾀｶﾅ"))
  (ert-info ("Hiragana / full-width alphabet")
    (evil-test-buffer
      "[ひ]らがなＡＢＣ"
      ("w")
      "ひらがな[Ａ]ＢＣ"))
  (ert-info ("Hiragana / full-width numeric")
    (evil-test-buffer
      "[ひ]らがな１２３"
      ("w")
      "ひらがな[１]２３"))
  (ert-info ("Hiragana / Hangul")
    (evil-test-buffer
      "[ひ]らがな한글"
      ("w")
      "ひらがな[한]글"))
  (ert-info ("Katakana / Latin")
    (evil-test-buffer
      "[カ]タカナabcd"
      ("w")
      "カタカナ[a]bcd"))
  (ert-info ("Katakana / numeric")
    (evil-test-buffer
      "[カ]タカナ1234"
      ("w")
      "カタカナ[1]234"))
  (ert-info ("Katakana / Kanji")
    (evil-test-buffer
      "[カ]タカナ漢字"
      ("w")
      "カタカナ[漢]字"))
  (ert-info ("Katakana / Hiragana")
    (evil-test-buffer
      "[カ]タカナひらがな"
      ("w")
      "カタカナ[ひ]らがな"))
  (ert-info ("Katakana / half-width Katakana")
    (evil-test-buffer
      "[カ]タカナｶﾀｶﾅ"
      ("w")
      "カタカナ[ｶ]ﾀｶﾅ"))
  (ert-info ("Katakana / full-width alphabet")
    (evil-test-buffer
      "[カ]タカナＡＢＣ"
      ("w")
      "カタカナ[Ａ]ＢＣ"))
  (ert-info ("Katakana / full-width numeric")
    (evil-test-buffer
      "[カ]タカナ１２３"
      ("w")
      "カタカナ[１]２３"))
  (ert-info ("Katakana / Hangul")
    (evil-test-buffer
      "[カ]タカナ한글"
      ("w")
      "カタカナ[한]글"))
  (ert-info ("half-width Katakana / Latin")
    (evil-test-buffer
      "[ｶ]ﾀｶﾅabcd"
      ("w")
      "ｶﾀｶﾅabc[d]"))
  (ert-info ("half-width Katakana / numeric")
    (evil-test-buffer
      "[ｶ]ﾀｶﾅ1234"
      ("w")
      "ｶﾀｶﾅ123[4]"))
  (ert-info ("half-width Katakana / Kanji")
    (evil-test-buffer
      "[ｶ]ﾀｶﾅ漢字"
      ("w")
      "ｶﾀｶﾅ[漢]字"))
  (ert-info ("half-width Katakana / Hiragana")
    (evil-test-buffer
      "[ｶ]ﾀｶﾅひらがな"
      ("w")
      "ｶﾀｶﾅ[ひ]らがな"))
  (ert-info ("half-width Katakana / Katakana")
    (evil-test-buffer
      "[ｶ]ﾀｶﾅカタカナ"
      ("w")
      "ｶﾀｶﾅ[カ]タカナ"))
  (ert-info ("half-width Katakana / full-width alphabet")
    (evil-test-buffer
      "[ｶ]ﾀｶﾅＡＢＣ"
      ("w")
      "ｶﾀｶﾅＡＢ[Ｃ]"))
  (ert-info ("half-width Katakana / full-width numeric")
    (evil-test-buffer
      "[ｶ]ﾀｶﾅ１２３"
      ("w")
      "ｶﾀｶﾅ１２[３]"))
  (ert-info ("half-width Katakana / Hangul")
    (evil-test-buffer
      "[ｶ]ﾀｶﾅ한글"
      ("w")
      "ｶﾀｶﾅ[한]글"))
  (ert-info ("full-width alphabet / Latin")
    (evil-test-buffer
      "[Ａ]ＢＣabcd"
      ("w")
      "ＡＢＣabc[d]"))
  (ert-info ("full-width alphabet / numeric")
    (evil-test-buffer
      "[Ａ]ＢＣ1234"
      ("w")
      "ＡＢＣ123[4]"))
  (ert-info ("full-width alphabet / Kanji")
    (evil-test-buffer
      "[Ａ]ＢＣ漢字"
      ("w")
      "ＡＢＣ[漢]字"))
  (ert-info ("full-width alphabet / Hiragana")
    (evil-test-buffer
      "[Ａ]ＢＣひらがな"
      ("w")
      "ＡＢＣ[ひ]らがな"))
  (ert-info ("full-width alphabet / Katakana")
    (evil-test-buffer
      "[Ａ]ＢＣカタカナ"
      ("w")
      "ＡＢＣ[カ]タカナ"))
  (ert-info ("full-width alphabet / half-width Katakana")
    (evil-test-buffer
      "[Ａ]ＢＣｶﾀｶﾅ"
      ("w")
      "ＡＢＣｶﾀｶ[ﾅ]"))
  (ert-info ("full-width alphabet / full-width numeric")
    (evil-test-buffer
      "[Ａ]ＢＣ１２３"
      ("w")
      "ＡＢＣ１２[３]"))
  (ert-info ("full-width alphabet / Hangul")
    (evil-test-buffer
      "[Ａ]ＢＣ한글"
      ("w")
      "ＡＢＣ[한]글"))
  (ert-info ("full-width numeric / Latin")
    (evil-test-buffer
      "[１]２３abcd"
      ("w")
      "１２３abc[d]"))
  (ert-info ("full-width numeric / numeric")
    (evil-test-buffer
      "[１]２３1234"
      ("w")
      "１２３123[4]"))
  (ert-info ("full-width numeric / Kanji")
    (evil-test-buffer
      "[１]２３漢字"
      ("w")
      "１２３[漢]字"))
  (ert-info ("full-width numeric / Hiragana")
    (evil-test-buffer
      "[１]２３ひらがな"
      ("w")
      "１２３[ひ]らがな"))
  (ert-info ("full-width numeric / Katakana")
    (evil-test-buffer
      "[１]２３カタカナ"
      ("w")
      "１２３[カ]タカナ"))
  (ert-info ("full-width numeric / half-width Katakana")
    (evil-test-buffer
      "[１]２３ｶﾀｶﾅ"
      ("w")
      "１２３ｶﾀｶ[ﾅ]"))
  (ert-info ("full-width numeric / full-width alphabet")
    (evil-test-buffer
      "[１]２３ＡＢＣ"
      ("w")
      "１２３ＡＢ[Ｃ]"))
  (ert-info ("full-width numeric / Hangul")
    (evil-test-buffer
      "[１]２３한글"
      ("w")
      "１２３[한]글"))
  (ert-info ("Hangul / Latin")
    (evil-test-buffer
      "[한]글abcd"
      ("w")
      "한글[a]bcd"))
  (ert-info ("Hangul / numeric")
    (evil-test-buffer
      "[한]글1234"
      ("w")
      "한글[1]234"))
  (ert-info ("Hangul / Kanji")
    (evil-test-buffer
      "[한]글漢字"
      ("w")
      "한글[漢]字"))
  (ert-info ("Hangul / Hiragana")
    (evil-test-buffer
      "[한]글ひらがな"
      ("w")
      "한글[ひ]らがな"))
  (ert-info ("Hangul / Katakana")
    (evil-test-buffer
      "[한]글カタカナ"
      ("w")
      "한글[カ]タカナ"))
  (ert-info ("Hangul / half-width Katakana")
    (evil-test-buffer
      "[한]글ｶﾀｶﾅ"
      ("w")
      "한글[ｶ]ﾀｶﾅ"))
  (ert-info ("Hangul / full-width alphabet")
    (evil-test-buffer
      "[한]글ＡＢＣ"
      ("w")
      "한글[Ａ]ＢＣ"))
  (ert-info ("Hangul / full-width numeric")
    (evil-test-buffer
      "[한]글１２３"
      ("w")
      "한글[１]２３")))

(ert-deftest evil-test-forward-word-end-cjk ()
  "Test `evil-forward-word-end' on CJK words"
  :tags '(evil motion cjk)
  (ert-info ("Latin / numeric")
    (evil-test-buffer
      "[a]bcd1234"
      ("e")
      "abcd123[4]"))
  (ert-info ("Latin / Kanji")
    (evil-test-buffer
      "[a]bcd漢字"
      ("e")
      "abc[d]漢字"))
  (ert-info ("Latin / Hiragana")
    (evil-test-buffer
      "[a]bcdひらがな"
      ("e")
      "abc[d]ひらがな"))
  (ert-info ("Latin / Katakana")
    (evil-test-buffer
      "[a]bcdカタカナ"
      ("e")
      "abc[d]カタカナ"))
  (ert-info ("Latin / half-width Katakana")
    (evil-test-buffer
      "[a]bcdｶﾀｶﾅ"
      ("e")
      "abcdｶﾀｶ[ﾅ]"))
  (ert-info ("Latin / full-width alphabet")
    (evil-test-buffer
      "[a]bcdＡＢＣ"
      ("e")
      "abcdＡＢ[Ｃ]"))
  (ert-info ("Latin / full-width numeric")
    (evil-test-buffer
      "[a]bcd１２３"
      ("e")
      "abcd１２[３]"))
  (ert-info ("Latin / Hangul")
    (evil-test-buffer
      "[a]bcd한글"
      ("e")
      "abc[d]한글"))
  (ert-info ("numeric / Latin")
    (evil-test-buffer
      "[1]234abcd"
      ("e")
      "1234abc[d]"))
  (ert-info ("numeric / Kanji")
    (evil-test-buffer
      "[1]234漢字"
      ("e")
      "123[4]漢字"))
  (ert-info ("numeric / Hiragana")
    (evil-test-buffer
      "[1]234ひらがな"
      ("e")
      "123[4]ひらがな"))
  (ert-info ("numeric / Katakana")
    (evil-test-buffer
      "[1]234カタカナ"
      ("e")
      "123[4]カタカナ"))
  (ert-info ("numeric / half-width Katakana")
    (evil-test-buffer
      "[1]234ｶﾀｶﾅ"
      ("e")
      "1234ｶﾀｶ[ﾅ]"))
  (ert-info ("numeric / full-width alphabet")
    (evil-test-buffer
      "[1]234ＡＢＣ"
      ("e")
      "1234ＡＢ[Ｃ]"))
  (ert-info ("numeric / full-width numeric")
    (evil-test-buffer
      "[1]234１２３"
      ("e")
      "1234１２[３]"))
  (ert-info ("numeric / Hangul")
    (evil-test-buffer
      "[1]234한글"
      ("e")
      "123[4]한글"))
  (ert-info ("Kanji / Latin")
    (evil-test-buffer
      "[漢]字abcd"
      ("e")
      "漢[字]abcd"))
  (ert-info ("Kanji / numeric")
    (evil-test-buffer
      "[漢]字1234"
      ("e")
      "漢[字]1234"))
  (ert-info ("Kanji / Hiragana")
    (evil-test-buffer
      "[漢]字ひらがな"
      ("e")
      "漢[字]ひらがな"))
  (ert-info ("Kanji / Katakana")
    (evil-test-buffer
      "[漢]字カタカナ"
      ("e")
      "漢[字]カタカナ"))
  (ert-info ("Kanji / half-width Katakana")
    (evil-test-buffer
      "[漢]字ｶﾀｶﾅ"
      ("e")
      "漢[字]ｶﾀｶﾅ"))
  (ert-info ("Kanji / full-width alphabet")
    (evil-test-buffer
      "[漢]字ＡＢＣ"
      ("e")
      "漢[字]ＡＢＣ"))
  (ert-info ("Kanji / full-width numeric")
    (evil-test-buffer
      "[漢]字１２３"
      ("e")
      "漢[字]１２３"))
  (ert-info ("Kanji / Hangul")
    (evil-test-buffer
      "[漢]字한글"
      ("e")
      "漢[字]한글"))
  (ert-info ("Hiragana / Latin")
    (evil-test-buffer
      "[ひ]らがなabcd"
      ("e")
      "ひらが[な]abcd"))
  (ert-info ("Hiragana / numeric")
    (evil-test-buffer
      "[ひ]らがな1234"
      ("e")
      "ひらが[な]1234"))
  (ert-info ("Hiragana / Kanji")
    (evil-test-buffer
      "[ひ]らがな漢字"
      ("e")
      "ひらが[な]漢字"))
  (ert-info ("Hiragana / Katakana")
    (evil-test-buffer
      "[ひ]らがなカタカナ"
      ("e")
      "ひらが[な]カタカナ"))
  (ert-info ("Hiragana / half-width Katakana")
    (evil-test-buffer
      "[ひ]らがなｶﾀｶﾅ"
      ("e")
      "ひらが[な]ｶﾀｶﾅ"))
  (ert-info ("Hiragana / full-width alphabet")
    (evil-test-buffer
      "[ひ]らがなＡＢＣ"
      ("e")
      "ひらが[な]ＡＢＣ"))
  (ert-info ("Hiragana / full-width numeric")
    (evil-test-buffer
      "[ひ]らがな１２３"
      ("e")
      "ひらが[な]１２３"))
  (ert-info ("Hiragana / Hangul")
    (evil-test-buffer
      "[ひ]らがな한글"
      ("e")
      "ひらが[な]한글"))
  (ert-info ("Katakana / Latin")
    (evil-test-buffer
      "[カ]タカナabcd"
      ("e")
      "カタカ[ナ]abcd"))
  (ert-info ("Katakana / numeric")
    (evil-test-buffer
      "[カ]タカナ1234"
      ("e")
      "カタカ[ナ]1234"))
  (ert-info ("Katakana / Kanji")
    (evil-test-buffer
      "[カ]タカナ漢字"
      ("e")
      "カタカ[ナ]漢字"))
  (ert-info ("Katakana / Hiragana")
    (evil-test-buffer
      "[カ]タカナひらがな"
      ("e")
      "カタカ[ナ]ひらがな"))
  (ert-info ("Katakana / half-width Katakana")
    (evil-test-buffer
      "[カ]タカナｶﾀｶﾅ"
      ("e")
      "カタカ[ナ]ｶﾀｶﾅ"))
  (ert-info ("Katakana / full-width alphabet")
    (evil-test-buffer
      "[カ]タカナＡＢＣ"
      ("e")
      "カタカ[ナ]ＡＢＣ"))
  (ert-info ("Katakana / full-width numeric")
    (evil-test-buffer
      "[カ]タカナ１２３"
      ("e")
      "カタカ[ナ]１２３"))
  (ert-info ("Katakana / Hangul")
    (evil-test-buffer
      "[カ]タカナ한글"
      ("e")
      "カタカ[ナ]한글"))
  (ert-info ("half-width Katakana / Latin")
    (evil-test-buffer
      "[ｶ]ﾀｶﾅabcd"
      ("e")
      "ｶﾀｶﾅabc[d]"))
  (ert-info ("half-width Katakana / numeric")
    (evil-test-buffer
      "[ｶ]ﾀｶﾅ1234"
      ("e")
      "ｶﾀｶﾅ123[4]"))
  (ert-info ("half-width Katakana / Kanji")
    (evil-test-buffer
      "[ｶ]ﾀｶﾅ漢字"
      ("e")
      "ｶﾀｶ[ﾅ]漢字"))
  (ert-info ("half-width Katakana / Hiragana")
    (evil-test-buffer
      "[ｶ]ﾀｶﾅひらがな"
      ("e")
      "ｶﾀｶ[ﾅ]ひらがな"))
  (ert-info ("half-width Katakana / Katakana")
    (evil-test-buffer
      "[ｶ]ﾀｶﾅカタカナ"
      ("e")
      "ｶﾀｶ[ﾅ]カタカナ"))
  (ert-info ("half-width Katakana / full-width alphabet")
    (evil-test-buffer
      "[ｶ]ﾀｶﾅＡＢＣ"
      ("e")
      "ｶﾀｶﾅＡＢ[Ｃ]"))
  (ert-info ("half-width Katakana / full-width numeric")
    (evil-test-buffer
      "[ｶ]ﾀｶﾅ１２３"
      ("e")
      "ｶﾀｶﾅ１２[３]"))
  (ert-info ("half-width Katakana / Hangul")
    (evil-test-buffer
      "[ｶ]ﾀｶﾅ한글"
      ("e")
      "ｶﾀｶ[ﾅ]한글"))
  (ert-info ("full-width alphabet / Latin")
    (evil-test-buffer
      "[Ａ]ＢＣabcd"
      ("e")
      "ＡＢＣabc[d]"))
  (ert-info ("full-width alphabet / numeric")
    (evil-test-buffer
      "[Ａ]ＢＣ1234"
      ("e")
      "ＡＢＣ123[4]"))
  (ert-info ("full-width alphabet / Kanji")
    (evil-test-buffer
      "[Ａ]ＢＣ漢字"
      ("e")
      "ＡＢ[Ｃ]漢字"))
  (ert-info ("full-width alphabet / Hiragana")
    (evil-test-buffer
      "[Ａ]ＢＣひらがな"
      ("e")
      "ＡＢ[Ｃ]ひらがな"))
  (ert-info ("full-width alphabet / Katakana")
    (evil-test-buffer
      "[Ａ]ＢＣカタカナ"
      ("e")
      "ＡＢ[Ｃ]カタカナ"))
  (ert-info ("full-width alphabet / half-width Katakana")
    (evil-test-buffer
      "[Ａ]ＢＣｶﾀｶﾅ"
      ("e")
      "ＡＢＣｶﾀｶ[ﾅ]"))
  (ert-info ("full-width alphabet / full-width numeric")
    (evil-test-buffer
      "[Ａ]ＢＣ１２３"
      ("e")
      "ＡＢＣ１２[３]"))
  (ert-info ("full-width alphabet / Hangul")
    (evil-test-buffer
      "[Ａ]ＢＣ한글"
      ("e")
      "ＡＢ[Ｃ]한글"))
  (ert-info ("full-width numeric / Latin")
    (evil-test-buffer
      "[１]２３abcd"
      ("e")
      "１２３abc[d]"))
  (ert-info ("full-width numeric / numeric")
    (evil-test-buffer
      "[１]２３1234"
      ("e")
      "１２３123[4]"))
  (ert-info ("full-width numeric / Kanji")
    (evil-test-buffer
      "[１]２３漢字"
      ("e")
      "１２[３]漢字"))
  (ert-info ("full-width numeric / Hiragana")
    (evil-test-buffer
      "[１]２３ひらがな"
      ("e")
      "１２[３]ひらがな"))
  (ert-info ("full-width numeric / Katakana")
    (evil-test-buffer
      "[１]２３カタカナ"
      ("e")
      "１２[３]カタカナ"))
  (ert-info ("full-width numeric / half-width Katakana")
    (evil-test-buffer
      "[１]２３ｶﾀｶﾅ"
      ("e")
      "１２３ｶﾀｶ[ﾅ]"))
  (ert-info ("full-width numeric / full-width alphabet")
    (evil-test-buffer
      "[１]２３ＡＢＣ"
      ("e")
      "１２３ＡＢ[Ｃ]"))
  (ert-info ("full-width numeric / Hangul")
    (evil-test-buffer
      "[１]２３한글"
      ("e")
      "１２[３]한글"))
  (ert-info ("Hangul / Latin")
    (evil-test-buffer
      "[한]글abcd"
      ("e")
      "한[글]abcd"))
  (ert-info ("Hangul / numeric")
    (evil-test-buffer
      "[한]글1234"
      ("e")
      "한[글]1234"))
  (ert-info ("Hangul / Kanji")
    (evil-test-buffer
      "[한]글漢字"
      ("e")
      "한[글]漢字"))
  (ert-info ("Hangul / Hiragana")
    (evil-test-buffer
      "[한]글ひらがな"
      ("e")
      "한[글]ひらがな"))
  (ert-info ("Hangul / Katakana")
    (evil-test-buffer
      "[한]글カタカナ"
      ("e")
      "한[글]カタカナ"))
  (ert-info ("Hangul / half-width Katakana")
    (evil-test-buffer
      "[한]글ｶﾀｶﾅ"
      ("e")
      "한[글]ｶﾀｶﾅ"))
  (ert-info ("Hangul / full-width alphabet")
    (evil-test-buffer
      "[한]글ＡＢＣ"
      ("e")
      "한[글]ＡＢＣ"))
  (ert-info ("Hangul / full-width numeric")
    (evil-test-buffer
      "[한]글１２３"
      ("e")
      "한[글]１２３")))

(ert-deftest evil-test-backword-word-begin-cjk ()
  "Test `evil-backward-word-begin' on CJK words"
  :tags '(evil motion cjk)
  (ert-info ("Latin / numeric")
    (evil-test-buffer
      "abcd123[4]"
      ("b")
      "[a]bcd1234"))
  (ert-info ("Latin / Kanji")
    (evil-test-buffer
      "abcd漢[字]"
      ("b")
      "abcd[漢]字"))
  (ert-info ("Latin / Hiragana")
    (evil-test-buffer
      "abcdひらが[な]"
      ("b")
      "abcd[ひ]らがな"))
  (ert-info ("Latin / Katakana")
    (evil-test-buffer
      "abcdカタカ[ナ]"
      ("b")
      "abcd[カ]タカナ"))
  (ert-info ("Latin / half-width Katakana")
    (evil-test-buffer
      "abcdｶﾀｶ[ﾅ]"
      ("b")
      "[a]bcdｶﾀｶﾅ"))
  (ert-info ("Latin / full-width alphabet")
    (evil-test-buffer
      "abcdＡＢ[Ｃ]"
      ("b")
      "[a]bcdＡＢＣ"))
  (ert-info ("Latin / full-width numeric")
    (evil-test-buffer
      "abcd１２[３]"
      ("b")
      "[a]bcd１２３"))
  (ert-info ("Latin / Hangul")
    (evil-test-buffer
      "abcd한[글]"
      ("b")
      "abcd[한]글"))
  (ert-info ("numeric / Latin")
    (evil-test-buffer
      "1234abc[d]"
      ("b")
      "[1]234abcd"))
  (ert-info ("numeric / Kanji")
    (evil-test-buffer
      "1234漢[字]"
      ("b")
      "1234[漢]字"))
  (ert-info ("numeric / Hiragana")
    (evil-test-buffer
      "1234ひらが[な]"
      ("b")
      "1234[ひ]らがな"))
  (ert-info ("numeric / Katakana")
    (evil-test-buffer
      "1234カタカ[ナ]"
      ("b")
      "1234[カ]タカナ"))
  (ert-info ("numeric / half-width Katakana")
    (evil-test-buffer
      "1234ｶﾀｶ[ﾅ]"
      ("b")
      "[1]234ｶﾀｶﾅ"))
  (ert-info ("numeric / full-width alphabet")
    (evil-test-buffer
      "1234ＡＢ[Ｃ]"
      ("b")
      "[1]234ＡＢＣ"))
  (ert-info ("numeric / full-width numeric")
    (evil-test-buffer
      "1234１２[３]"
      ("b")
      "[1]234１２３"))
  (ert-info ("numeric / Hangul")
    (evil-test-buffer
      "1234한[글]"
      ("b")
      "1234[한]글"))
  (ert-info ("Kanji / Latin")
    (evil-test-buffer
      "漢字abc[d]"
      ("b")
      "漢字[a]bcd"))
  (ert-info ("Kanji / numeric")
    (evil-test-buffer
      "漢字123[4]"
      ("b")
      "漢字[1]234"))
  (ert-info ("Kanji / Hiragana")
    (evil-test-buffer
      "漢字ひらが[な]"
      ("b")
      "漢字[ひ]らがな"))
  (ert-info ("Kanji / Katakana")
    (evil-test-buffer
      "漢字カタカ[ナ]"
      ("b")
      "漢字[カ]タカナ"))
  (ert-info ("Kanji / half-width Katakana")
    (evil-test-buffer
      "漢字ｶﾀｶ[ﾅ]"
      ("b")
      "漢字[ｶ]ﾀｶﾅ"))
  (ert-info ("Kanji / full-width alphabet")
    (evil-test-buffer
      "漢字ＡＢ[Ｃ]"
      ("b")
      "漢字[Ａ]ＢＣ"))
  (ert-info ("Kanji / full-width numeric")
    (evil-test-buffer
      "漢字１２[３]"
      ("b")
      "漢字[１]２３"))
  (ert-info ("Kanji / Hangul")
    (evil-test-buffer
      "漢字한[글]"
      ("b")
      "漢字[한]글"))
  (ert-info ("Hiragana / Latin")
    (evil-test-buffer
      "ひらがなabc[d]"
      ("b")
      "ひらがな[a]bcd"))
  (ert-info ("Hiragana / numeric")
    (evil-test-buffer
      "ひらがな123[4]"
      ("b")
      "ひらがな[1]234"))
  (ert-info ("Hiragana / Kanji")
    (evil-test-buffer
      "ひらがな漢[字]"
      ("b")
      "ひらがな[漢]字"))
  (ert-info ("Hiragana / Katakana")
    (evil-test-buffer
      "ひらがなカタカ[ナ]"
      ("b")
      "ひらがな[カ]タカナ"))
  (ert-info ("Hiragana / half-width Katakana")
    (evil-test-buffer
      "ひらがなｶﾀｶ[ﾅ]"
      ("b")
      "ひらがな[ｶ]ﾀｶﾅ"))
  (ert-info ("Hiragana / full-width alphabet")
    (evil-test-buffer
      "ひらがなＡＢ[Ｃ]"
      ("b")
      "ひらがな[Ａ]ＢＣ"))
  (ert-info ("Hiragana / full-width numeric")
    (evil-test-buffer
      "ひらがな１２[３]"
      ("b")
      "ひらがな[１]２３"))
  (ert-info ("Hiragana / Hangul")
    (evil-test-buffer
      "ひらがな한[글]"
      ("b")
      "ひらがな[한]글"))
  (ert-info ("Katakana / Latin")
    (evil-test-buffer
      "カタカナabc[d]"
      ("b")
      "カタカナ[a]bcd"))
  (ert-info ("Katakana / numeric")
    (evil-test-buffer
      "カタカナ123[4]"
      ("b")
      "カタカナ[1]234"))
  (ert-info ("Katakana / Kanji")
    (evil-test-buffer
      "カタカナ漢[字]"
      ("b")
      "カタカナ[漢]字"))
  (ert-info ("Katakana / Hiragana")
    (evil-test-buffer
      "カタカナひらが[な]"
      ("b")
      "カタカナ[ひ]らがな"))
  (ert-info ("Katakana / half-width Katakana")
    (evil-test-buffer
      "カタカナｶﾀｶ[ﾅ]"
      ("b")
      "カタカナ[ｶ]ﾀｶﾅ"))
  (ert-info ("Katakana / full-width alphabet")
    (evil-test-buffer
      "カタカナＡＢ[Ｃ]"
      ("b")
      "カタカナ[Ａ]ＢＣ"))
  (ert-info ("Katakana / full-width numeric")
    (evil-test-buffer
      "カタカナ１２[３]"
      ("b")
      "カタカナ[１]２３"))
  (ert-info ("Katakana / Hangul")
    (evil-test-buffer
      "カタカナ한[글]"
      ("b")
      "カタカナ[한]글"))
  (ert-info ("half-width Katakana / Latin")
    (evil-test-buffer
      "ｶﾀｶﾅabc[d]"
      ("b")
      "[ｶ]ﾀｶﾅabcd"))
  (ert-info ("half-width Katakana / numeric")
    (evil-test-buffer
      "ｶﾀｶﾅ123[4]"
      ("b")
      "[ｶ]ﾀｶﾅ1234"))
  (ert-info ("half-width Katakana / Kanji")
    (evil-test-buffer
      "ｶﾀｶﾅ漢[字]"
      ("b")
      "ｶﾀｶﾅ[漢]字"))
  (ert-info ("half-width Katakana / Hiragana")
    (evil-test-buffer
      "ｶﾀｶﾅひらが[な]"
      ("b")
      "ｶﾀｶﾅ[ひ]らがな"))
  (ert-info ("half-width Katakana / Katakana")
    (evil-test-buffer
      "ｶﾀｶﾅカタカ[ナ]"
      ("b")
      "ｶﾀｶﾅ[カ]タカナ"))
  (ert-info ("half-width Katakana / full-width alphabet")
    (evil-test-buffer
      "ｶﾀｶﾅＡＢ[Ｃ]"
      ("b")
      "[ｶ]ﾀｶﾅＡＢＣ"))
  (ert-info ("half-width Katakana / full-width numeric")
    (evil-test-buffer
      "ｶﾀｶﾅ１２[３]"
      ("b")
      "[ｶ]ﾀｶﾅ１２３"))
  (ert-info ("half-width Katakana / Hangul")
    (evil-test-buffer
      "ｶﾀｶﾅ한[글]"
      ("b")
      "ｶﾀｶﾅ[한]글"))
  (ert-info ("full-width alphabet / Latin")
    (evil-test-buffer
      "ＡＢＣabc[d]"
      ("b")
      "[Ａ]ＢＣabcd"))
  (ert-info ("full-width alphabet / numeric")
    (evil-test-buffer
      "ＡＢＣ123[4]"
      ("b")
      "[Ａ]ＢＣ1234"))
  (ert-info ("full-width alphabet / Kanji")
    (evil-test-buffer
      "ＡＢＣ漢[字]"
      ("b")
      "ＡＢＣ[漢]字"))
  (ert-info ("full-width alphabet / Hiragana")
    (evil-test-buffer
      "ＡＢＣひらが[な]"
      ("b")
      "ＡＢＣ[ひ]らがな"))
  (ert-info ("full-width alphabet / Katakana")
    (evil-test-buffer
      "ＡＢＣカタカ[ナ]"
      ("b")
      "ＡＢＣ[カ]タカナ"))
  (ert-info ("full-width alphabet / half-width Katakana")
    (evil-test-buffer
      "ＡＢＣｶﾀｶ[ﾅ]"
      ("b")
      "[Ａ]ＢＣｶﾀｶﾅ"))
  (ert-info ("full-width alphabet / full-width numeric")
    (evil-test-buffer
      "ＡＢＣ１２[３]"
      ("b")
      "[Ａ]ＢＣ１２３"))
  (ert-info ("full-width alphabet / Hangul")
    (evil-test-buffer
      "ＡＢＣ한[글]"
      ("b")
      "ＡＢＣ[한]글"))
  (ert-info ("full-width numeric / Latin")
    (evil-test-buffer
      "１２３abc[d]"
      ("b")
      "[１]２３abcd"))
  (ert-info ("full-width numeric / numeric")
    (evil-test-buffer
      "１２３123[4]"
      ("b")
      "[１]２３1234"))
  (ert-info ("full-width numeric / Kanji")
    (evil-test-buffer
      "１２３漢[字]"
      ("b")
      "１２３[漢]字"))
  (ert-info ("full-width numeric / Hiragana")
    (evil-test-buffer
      "１２３ひらが[な]"
      ("b")
      "１２３[ひ]らがな"))
  (ert-info ("full-width numeric / Katakana")
    (evil-test-buffer
      "１２３カタカ[ナ]"
      ("b")
      "１２３[カ]タカナ"))
  (ert-info ("full-width numeric / half-width Katakana")
    (evil-test-buffer
      "１２３ｶﾀｶ[ﾅ]"
      ("b")
      "[１]２３ｶﾀｶﾅ"))
  (ert-info ("full-width numeric / full-width alphabet")
    (evil-test-buffer
      "１２３ＡＢ[Ｃ]"
      ("b")
      "[１]２３ＡＢＣ"))
  (ert-info ("full-width numeric / Hangul")
    (evil-test-buffer
      "１２３한[글]"
      ("b")
      "１２３[한]글"))
  (ert-info ("Hangul / Latin")
    (evil-test-buffer
      "한글abc[d]"
      ("b")
      "한글[a]bcd"))
  (ert-info ("Hangul / numeric")
    (evil-test-buffer
      "한글123[4]"
      ("b")
      "한글[1]234"))
  (ert-info ("Hangul / Kanji")
    (evil-test-buffer
      "한글漢[字]"
      ("b")
      "한글[漢]字"))
  (ert-info ("Hangul / Hiragana")
    (evil-test-buffer
      "한글ひらが[な]"
      ("b")
      "한글[ひ]らがな"))
  (ert-info ("Hangul / Katakana")
    (evil-test-buffer
      "한글カタカ[ナ]"
      ("b")
      "한글[カ]タカナ"))
  (ert-info ("Hangul / half-width Katakana")
    (evil-test-buffer
      "한글ｶﾀｶ[ﾅ]"
      ("b")
      "한글[ｶ]ﾀｶﾅ"))
  (ert-info ("Hangul / full-width alphabet")
    (evil-test-buffer
      "한글ＡＢ[Ｃ]"
      ("b")
      "한글[Ａ]ＢＣ"))
  (ert-info ("Hangul / full-width numeric")
    (evil-test-buffer
      "한글１２[３]"
      ("b")
      "한글[１]２３")))

(ert-deftest evil-test-backword-word-end-cjk ()
  "Test `evil-backward-word-end' on CJK words"
  :tags '(evil motion cjk)
  (ert-info ("Latin / numeric")
    (evil-test-buffer
      "abcd123[4]"
      ("ge")
      "[a]bcd1234"))
  (ert-info ("Latin / Kanji")
    (evil-test-buffer
      "abcd漢[字]"
      ("ge")
      "abc[d]漢字"))
  (ert-info ("Latin / Hiragana")
    (evil-test-buffer
      "abcdひらが[な]"
      ("ge")
      "abc[d]ひらがな"))
  (ert-info ("Latin / Katakana")
    (evil-test-buffer
      "abcdカタカ[ナ]"
      ("ge")
      "abc[d]カタカナ"))
  (ert-info ("Latin / half-width Katakana")
    (evil-test-buffer
      "abcdｶﾀｶ[ﾅ]"
      ("ge")
      "[a]bcdｶﾀｶﾅ"))
  (ert-info ("Latin / full-width alphabet")
    (evil-test-buffer
      "abcdＡＢ[Ｃ]"
      ("ge")
      "[a]bcdＡＢＣ"))
  (ert-info ("Latin / full-width numeric")
    (evil-test-buffer
      "abcd１２[３]"
      ("ge")
      "[a]bcd１２３"))
  (ert-info ("Latin / Hangul")
    (evil-test-buffer
      "abcd한[글]"
      ("ge")
      "abc[d]한글"))
  (ert-info ("numeric / Latin")
    (evil-test-buffer
      "1234abc[d]"
      ("ge")
      "[1]234abcd"))
  (ert-info ("numeric / Kanji")
    (evil-test-buffer
      "1234漢[字]"
      ("ge")
      "123[4]漢字"))
  (ert-info ("numeric / Hiragana")
    (evil-test-buffer
      "1234ひらが[な]"
      ("ge")
      "123[4]ひらがな"))
  (ert-info ("numeric / Katakana")
    (evil-test-buffer
      "1234カタカ[ナ]"
      ("ge")
      "123[4]カタカナ"))
  (ert-info ("numeric / half-width Katakana")
    (evil-test-buffer
      "1234ｶﾀｶ[ﾅ]"
      ("ge")
      "[1]234ｶﾀｶﾅ"))
  (ert-info ("numeric / full-width alphabet")
    (evil-test-buffer
      "1234ＡＢ[Ｃ]"
      ("ge")
      "[1]234ＡＢＣ"))
  (ert-info ("numeric / full-width numeric")
    (evil-test-buffer
      "1234１２[３]"
      ("ge")
      "[1]234１２３"))
  (ert-info ("numeric / Hangul")
    (evil-test-buffer
      "1234한[글]"
      ("ge")
      "123[4]한글"))
  (ert-info ("Kanji / Latin")
    (evil-test-buffer
      "漢字abc[d]"
      ("ge")
      "漢[字]abcd"))
  (ert-info ("Kanji / numeric")
    (evil-test-buffer
      "漢字123[4]"
      ("ge")
      "漢[字]1234"))
  (ert-info ("Kanji / Hiragana")
    (evil-test-buffer
      "漢字ひらが[な]"
      ("ge")
      "漢[字]ひらがな"))
  (ert-info ("Kanji / Katakana")
    (evil-test-buffer
      "漢字カタカ[ナ]"
      ("ge")
      "漢[字]カタカナ"))
  (ert-info ("Kanji / half-width Katakana")
    (evil-test-buffer
      "漢字ｶﾀｶ[ﾅ]"
      ("ge")
      "漢[字]ｶﾀｶﾅ"))
  (ert-info ("Kanji / full-width alphabet")
    (evil-test-buffer
      "漢字ＡＢ[Ｃ]"
      ("ge")
      "漢[字]ＡＢＣ"))
  (ert-info ("Kanji / full-width numeric")
    (evil-test-buffer
      "漢字１２[３]"
      ("ge")
      "漢[字]１２３"))
  (ert-info ("Kanji / Hangul")
    (evil-test-buffer
      "漢字한[글]"
      ("ge")
      "漢[字]한글"))
  (ert-info ("Hiragana / Latin")
    (evil-test-buffer
      "ひらがなabc[d]"
      ("ge")
      "ひらが[な]abcd"))
  (ert-info ("Hiragana / numeric")
    (evil-test-buffer
      "ひらがな123[4]"
      ("ge")
      "ひらが[な]1234"))
  (ert-info ("Hiragana / Kanji")
    (evil-test-buffer
      "ひらがな漢[字]"
      ("ge")
      "ひらが[な]漢字"))
  (ert-info ("Hiragana / Katakana")
    (evil-test-buffer
      "ひらがなカタカ[ナ]"
      ("ge")
      "ひらが[な]カタカナ"))
  (ert-info ("Hiragana / half-width Katakana")
    (evil-test-buffer
      "ひらがなｶﾀｶ[ﾅ]"
      ("ge")
      "ひらが[な]ｶﾀｶﾅ"))
  (ert-info ("Hiragana / full-width alphabet")
    (evil-test-buffer
      "ひらがなＡＢ[Ｃ]"
      ("ge")
      "ひらが[な]ＡＢＣ"))
  (ert-info ("Hiragana / full-width numeric")
    (evil-test-buffer
      "ひらがな１２[３]"
      ("ge")
      "ひらが[な]１２３"))
  (ert-info ("Hiragana / Hangul")
    (evil-test-buffer
      "ひらがな한[글]"
      ("ge")
      "ひらが[な]한글"))
  (ert-info ("Katakana / Latin")
    (evil-test-buffer
      "カタカナabc[d]"
      ("ge")
      "カタカ[ナ]abcd"))
  (ert-info ("Katakana / numeric")
    (evil-test-buffer
      "カタカナ123[4]"
      ("ge")
      "カタカ[ナ]1234"))
  (ert-info ("Katakana / Kanji")
    (evil-test-buffer
      "カタカナ漢[字]"
      ("ge")
      "カタカ[ナ]漢字"))
  (ert-info ("Katakana / Hiragana")
    (evil-test-buffer
      "カタカナひらが[な]"
      ("ge")
      "カタカ[ナ]ひらがな"))
  (ert-info ("Katakana / half-width Katakana")
    (evil-test-buffer
      "カタカナｶﾀｶ[ﾅ]"
      ("ge")
      "カタカ[ナ]ｶﾀｶﾅ"))
  (ert-info ("Katakana / full-width alphabet")
    (evil-test-buffer
      "カタカナＡＢ[Ｃ]"
      ("ge")
      "カタカ[ナ]ＡＢＣ"))
  (ert-info ("Katakana / full-width numeric")
    (evil-test-buffer
      "カタカナ１２[３]"
      ("ge")
      "カタカ[ナ]１２３"))
  (ert-info ("Katakana / Hangul")
    (evil-test-buffer
      "カタカナ한[글]"
      ("ge")
      "カタカ[ナ]한글"))
  (ert-info ("half-width Katakana / Latin")
    (evil-test-buffer
      "ｶﾀｶﾅabc[d]"
      ("ge")
      "[ｶ]ﾀｶﾅabcd"))
  (ert-info ("half-width Katakana / numeric")
    (evil-test-buffer
      "ｶﾀｶﾅ123[4]"
      ("ge")
      "[ｶ]ﾀｶﾅ1234"))
  (ert-info ("half-width Katakana / Kanji")
    (evil-test-buffer
      "ｶﾀｶﾅ漢[字]"
      ("ge")
      "ｶﾀｶ[ﾅ]漢字"))
  (ert-info ("half-width Katakana / Hiragana")
    (evil-test-buffer
      "ｶﾀｶﾅひらが[な]"
      ("ge")
      "ｶﾀｶ[ﾅ]ひらがな"))
  (ert-info ("half-width Katakana / Katakana")
    (evil-test-buffer
      "ｶﾀｶﾅカタカ[ナ]"
      ("ge")
      "ｶﾀｶ[ﾅ]カタカナ"))
  (ert-info ("half-width Katakana / full-width alphabet")
    (evil-test-buffer
      "ｶﾀｶﾅＡＢ[Ｃ]"
      ("ge")
      "[ｶ]ﾀｶﾅＡＢＣ"))
  (ert-info ("half-width Katakana / full-width numeric")
    (evil-test-buffer
      "ｶﾀｶﾅ１２[３]"
      ("ge")
      "[ｶ]ﾀｶﾅ１２３"))
  (ert-info ("half-width Katakana / Hangul")
    (evil-test-buffer
      "ｶﾀｶﾅ한[글]"
      ("ge")
      "ｶﾀｶ[ﾅ]한글"))
  (ert-info ("full-width alphabet / Latin")
    (evil-test-buffer
      "ＡＢＣabc[d]"
      ("ge")
      "[Ａ]ＢＣabcd"))
  (ert-info ("full-width alphabet / numeric")
    (evil-test-buffer
      "ＡＢＣ123[4]"
      ("ge")
      "[Ａ]ＢＣ1234"))
  (ert-info ("full-width alphabet / Kanji")
    (evil-test-buffer
      "ＡＢＣ漢[字]"
      ("ge")
      "ＡＢ[Ｃ]漢字"))
  (ert-info ("full-width alphabet / Hiragana")
    (evil-test-buffer
      "ＡＢＣひらが[な]"
      ("ge")
      "ＡＢ[Ｃ]ひらがな"))
  (ert-info ("full-width alphabet / Katakana")
    (evil-test-buffer
      "ＡＢＣカタカ[ナ]"
      ("ge")
      "ＡＢ[Ｃ]カタカナ"))
  (ert-info ("full-width alphabet / half-width Katakana")
    (evil-test-buffer
      "ＡＢＣｶﾀｶ[ﾅ]"
      ("ge")
      "[Ａ]ＢＣｶﾀｶﾅ"))
  (ert-info ("full-width alphabet / full-width numeric")
    (evil-test-buffer
      "ＡＢＣ１２[３]"
      ("ge")
      "[Ａ]ＢＣ１２３"))
  (ert-info ("full-width alphabet / Hangul")
    (evil-test-buffer
      "ＡＢＣ한[글]"
      ("ge")
      "ＡＢ[Ｃ]한글"))
  (ert-info ("full-width numeric / Latin")
    (evil-test-buffer
      "１２３abc[d]"
      ("ge")
      "[１]２３abcd"))
  (ert-info ("full-width numeric / numeric")
    (evil-test-buffer
      "１２３123[4]"
      ("ge")
      "[１]２３1234"))
  (ert-info ("full-width numeric / Kanji")
    (evil-test-buffer
      "１２３漢[字]"
      ("ge")
      "１２[３]漢字"))
  (ert-info ("full-width numeric / Hiragana")
    (evil-test-buffer
      "１２３ひらが[な]"
      ("ge")
      "１２[３]ひらがな"))
  (ert-info ("full-width numeric / Katakana")
    (evil-test-buffer
      "１２３カタカ[ナ]"
      ("ge")
      "１２[３]カタカナ"))
  (ert-info ("full-width numeric / half-width Katakana")
    (evil-test-buffer
      "１２３ｶﾀｶ[ﾅ]"
      ("ge")
      "[１]２３ｶﾀｶﾅ"))
  (ert-info ("full-width numeric / full-width alphabet")
    (evil-test-buffer
      "１２３ＡＢ[Ｃ]"
      ("ge")
      "[１]２３ＡＢＣ"))
  (ert-info ("full-width numeric / Hangul")
    (evil-test-buffer
      "１２３한[글]"
      ("ge")
      "１２[３]한글"))
  (ert-info ("Hangul / Latin")
    (evil-test-buffer
      "한글abc[d]"
      ("ge")
      "한[글]abcd"))
  (ert-info ("Hangul / numeric")
    (evil-test-buffer
      "한글123[4]"
      ("ge")
      "한[글]1234"))
  (ert-info ("Hangul / Kanji")
    (evil-test-buffer
      "한글漢[字]"
      ("ge")
      "한[글]漢字"))
  (ert-info ("Hangul / Hiragana")
    (evil-test-buffer
      "한글ひらが[な]"
      ("ge")
      "한[글]ひらがな"))
  (ert-info ("Hangul / Katakana")
    (evil-test-buffer
      "한글カタカ[ナ]"
      ("ge")
      "한[글]カタカナ"))
  (ert-info ("Hangul / half-width Katakana")
    (evil-test-buffer
      "한글ｶﾀｶ[ﾅ]"
      ("ge")
      "한[글]ｶﾀｶﾅ"))
  (ert-info ("Hangul / full-width alphabet")
    (evil-test-buffer
      "한글ＡＢ[Ｃ]"
      ("ge")
      "한[글]ＡＢＣ"))
  (ert-info ("Hangul / full-width numeric")
    (evil-test-buffer
      "한글１２[３]"
      ("ge")
      "한[글]１２３")))

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
      (")")
      "Below some empty line.\n[\n]"
      (")")
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
  (ert-info ("Repeat should skip adjacent character")
    (let ((evil-repeat-find-to-skip-next t))
      (evil-test-buffer
        "[a]aaxaaaxaaaxaaa"
        ("tx;")
        "aaaxaa[a]xaaaxaaa"
        (";")
        "aaaxaaaxaa[a]xaaa"
        (",")
        "aaaxaaax[a]aaxaaa"
        (",")
        "aaax[a]aaxaaaxaaa")))
  (ert-info ("Repeat should NOT skip adjacent character")
    (let ((evil-repeat-find-to-skip-next nil))
      (evil-test-buffer
        "[a]aaxaaaxaaaxaaa"
        ("tx;")
        "aa[a]xaaaxaaaxaaa")))
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
  (ert-info ("Repeat should skip adjacent character")
    (let ((evil-repeat-find-to-skip-next t))
      (evil-test-buffer
        "aaaxaaaxaaaxaa[a]"
        ("Tx;")
        "aaaxaaax[a]aaxaaa"
        (";")
        "aaax[a]aaxaaaxaaa"
        (",")
        "aaaxaa[a]xaaaxaaa"
        (",")
        "aaaxaaaxaa[a]xaaa")))
  (ert-info ("Repeat should NOT skip adjacent character")
    (let ((evil-repeat-find-to-skip-next nil))
      (evil-test-buffer
        "aaaxaaaxaaaxaa[a]"
        ("Tx;")
        "aaaxaaaxaaax[a]aa")))
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
      (should-error (execute-kbd-macro "%"))))
  (ert-info ("Before unmatched opening parenthesies")
    (evil-test-buffer
      "x[x]xx ( yyyyy () zzzz"
      (should-error (execute-kbd-macro "%"))
      "x[x]xx ( yyyyy () zzzz"))
  (ert-info ("Before unmatched closing parenthesies")
    (evil-test-buffer
      "x[x]xx ) yyyyy () zzzz"
      (should-error (execute-kbd-macro "%"))
      "x[x]xx ) yyyyy () zzzz"))

  (ert-info ("At the end of the line")
    (evil-test-buffer
      "[p]ublic void foo(String bar) {\n   blabla;\n}\n"
      ("v$%")
      "public void foo(String bar) {\n   blabla;\n[}]\n")))

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

(ert-deftest evil-test-flyspell-motions ()
  "Test flyspell motions"
  :tags '(evil motion)
  (ert-info ("Simple")
    (evil-test-buffer
      "[I] cannt tpye for lyfe"
      (flyspell-mode)
      (flyspell-buffer)
      ("]s")
      "I [c]annt tpye for lyfe"
      ("]s")
      "I cannt [t]pye for lyfe"
      ("]s")
      "I cannt tpye for [l]yfe"
      ("]s")
      "I [c]annt tpye for lyfe"
      ("[s")
      "I cannt tpye for [l]yfe"
      ("[s")
      "I cannt [t]pye for lyfe"))
  (ert-info ("With count")
    (evil-test-buffer
      "[I] cannt tpye for lyfe"
      (flyspell-mode)
      (flyspell-buffer)
      ("2]s")
      "I cannt [t]pye for lyfe"
      ("2]s")
      "I [c]annt tpye for lyfe"
      ("2[s")
      "I cannt [t]pye for lyfe"
      ("2[s")
      "I cannt tpye for [l]yfe"))
  (ert-info ("With evil-search-wrap disabled")
    (let (evil-search-wrap)
      (evil-test-buffer
       "[I] cannt tpye for lyfe"
       (flyspell-mode)
       (flyspell-buffer)
       ("]s")
       "I [c]annt tpye for lyfe"
       ("]s")
       "I cannt [t]pye for lyfe"
       ("]s")
       "I cannt tpye for [l]yfe"
       ("]s")
       "I cannt tpye for [l]yfe")))
  (ert-info ("One mistake")
    (evil-test-buffer
     "[I]'m almst there..."
     (flyspell-mode)
     (flyspell-buffer)
      ("]s")
     "I'm [a]lmst there..."
      ("]s")
     "I'm [a]lmst there..."))
  (ert-info ("No mistakes")
    (evil-test-buffer
     "[I]'ve learned to type!"
     (flyspell-mode)
     (flyspell-buffer)
      ("]s")
     "[I]'ve learned to type!"
      ("[s")
     "[I]'ve learned to type!")))

;;; Text objects

(ert-deftest evil-test-text-object ()
  "Test `evil-define-text-object'"
  :tags '(evil text-object)
  (let ((object (evil-define-text-object nil (count &optional beg end type)
                  (let ((sel (and beg end (evil-range beg end))))
                    (when (and sel (> count 0)) (forward-char 1))
                    (let ((range (if (< count 0)
                                     (list (- (point) 3) (point))
                                   (list (point) (+ (point) 3)))))
                      (if sel
                          (evil-range-union range sel)
                        range))))))
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
      ";; <This[ ]>buffer is for notes.")
    (evil-test-buffer
      ";; Thi[s] buffer is for notes."
      ("viw")
      ";; <Thi[s]> buffer is for notes.")
    (evil-test-buffer
      ";; Thi[s] buffer is for notes."
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
        ";;<[ ]This buffer> is for notes.")))
  (ert-info ("select first visual word")
    (evil-test-buffer
      "([a])"
      ("viw")
      "(<[a]>)")))

(ert-deftest evil-test-word-objects-cjk ()
  "Test `evil-inner-word' and `evil-a-word' on CJK words"
  :tags '(evil text-object cjk)
  (ert-info ("Select a word")
    (evil-test-buffer
      "[a]bcd1234"
      ("viw")
      "<abcd123[4]>")
    (evil-test-buffer
      "[a]bcd1234"
      ("vaw")
      "<abcd123[4]>")
    (evil-test-buffer
      "[a]bcd漢字"
      ("viw")
      "<abc[d]>漢字")
    (evil-test-buffer
      "[a]bcd漢字"
      ("vaw")
      "<abc[d]>漢字")
    (evil-test-buffer
      "[a]bcdひらがな"
      ("viw")
      "<abc[d]>ひらがな")
    (evil-test-buffer
      "[a]bcdひらがな"
      ("vaw")
      "<abc[d]>ひらがな")
    (evil-test-buffer
      "[a]bcdカタカナ"
      ("viw")
      "<abc[d]>カタカナ")
    (evil-test-buffer
      "[a]bcdカタカナ"
      ("vaw")
      "<abc[d]>カタカナ")
    (evil-test-buffer
      "[a]bcdｶﾀｶﾅ"
      ("viw")
      "<abcdｶﾀｶ[ﾅ]>")
    (evil-test-buffer
      "[a]bcdｶﾀｶﾅ"
      ("vaw")
      "<abcdｶﾀｶ[ﾅ]>")
    (evil-test-buffer
      "[a]bcdＡＢＣ"
      ("viw")
      "<abcdＡＢ[Ｃ]>")
    (evil-test-buffer
      "[a]bcdＡＢＣ"
      ("vaw")
      "<abcdＡＢ[Ｃ]>")
    (evil-test-buffer
      "[a]bcd１２３"
      ("viw")
      "<abcd１２[３]>")
    (evil-test-buffer
      "[a]bcd１２３"
      ("vaw")
      "<abcd１２[３]>")
    (evil-test-buffer
      "[a]bcd한글"
      ("viw")
      "<abc[d]>한글")
    (evil-test-buffer
      "[a]bcd한글"
      ("vaw")
      "<abc[d]>한글")
    (evil-test-buffer
      "[1]234abcd"
      ("viw")
      "<1234abc[d]>")
    (evil-test-buffer
      "[1]234abcd"
      ("vaw")
      "<1234abc[d]>")
    (evil-test-buffer
      "[1]234漢字"
      ("viw")
      "<123[4]>漢字")
    (evil-test-buffer
      "[1]234漢字"
      ("vaw")
      "<123[4]>漢字")
    (evil-test-buffer
      "[1]234ひらがな"
      ("viw")
      "<123[4]>ひらがな")
    (evil-test-buffer
      "[1]234ひらがな"
      ("vaw")
      "<123[4]>ひらがな")
    (evil-test-buffer
      "[1]234カタカナ"
      ("viw")
      "<123[4]>カタカナ")
    (evil-test-buffer
      "[1]234カタカナ"
      ("vaw")
      "<123[4]>カタカナ")
    (evil-test-buffer
      "[1]234ｶﾀｶﾅ"
      ("viw")
      "<1234ｶﾀｶ[ﾅ]>")
    (evil-test-buffer
      "[1]234ｶﾀｶﾅ"
      ("vaw")
      "<1234ｶﾀｶ[ﾅ]>")
    (evil-test-buffer
      "[1]234ＡＢＣ"
      ("viw")
      "<1234ＡＢ[Ｃ]>")
    (evil-test-buffer
      "[1]234ＡＢＣ"
      ("vaw")
      "<1234ＡＢ[Ｃ]>")
    (evil-test-buffer
      "[1]234１２３"
      ("viw")
      "<1234１２[３]>")
    (evil-test-buffer
      "[1]234１２３"
      ("vaw")
      "<1234１２[３]>")
    (evil-test-buffer
      "[1]234한글"
      ("viw")
      "<123[4]>한글")
    (evil-test-buffer
      "[1]234한글"
      ("vaw")
      "<123[4]>한글")
    (evil-test-buffer
      "[漢]字abcd"
      ("viw")
      "<漢[字]>abcd")
    (evil-test-buffer
      "[漢]字abcd"
      ("vaw")
      "<漢[字]>abcd")
    (evil-test-buffer
      "[漢]字1234"
      ("viw")
      "<漢[字]>1234")
    (evil-test-buffer
      "[漢]字1234"
      ("vaw")
      "<漢[字]>1234")
    (evil-test-buffer
      "[漢]字ひらがな"
      ("viw")
      "<漢[字]>ひらがな")
    (evil-test-buffer
      "[漢]字ひらがな"
      ("vaw")
      "<漢[字]>ひらがな")
    (evil-test-buffer
      "[漢]字カタカナ"
      ("viw")
      "<漢[字]>カタカナ")
    (evil-test-buffer
      "[漢]字カタカナ"
      ("vaw")
      "<漢[字]>カタカナ")
    (evil-test-buffer
      "[漢]字ｶﾀｶﾅ"
      ("viw")
      "<漢[字]>ｶﾀｶﾅ")
    (evil-test-buffer
      "[漢]字ｶﾀｶﾅ"
      ("vaw")
      "<漢[字]>ｶﾀｶﾅ")
    (evil-test-buffer
      "[漢]字ＡＢＣ"
      ("viw")
      "<漢[字]>ＡＢＣ")
    (evil-test-buffer
      "[漢]字ＡＢＣ"
      ("vaw")
      "<漢[字]>ＡＢＣ")
    (evil-test-buffer
      "[漢]字１２３"
      ("viw")
      "<漢[字]>１２３")
    (evil-test-buffer
      "[漢]字１２３"
      ("vaw")
      "<漢[字]>１２３")
    (evil-test-buffer
      "[漢]字한글"
      ("viw")
      "<漢[字]>한글")
    (evil-test-buffer
      "[漢]字한글"
      ("vaw")
      "<漢[字]>한글")
    (evil-test-buffer
      "[ひ]らがなabcd"
      ("viw")
      "<ひらが[な]>abcd")
    (evil-test-buffer
      "[ひ]らがなabcd"
      ("vaw")
      "<ひらが[な]>abcd")
    (evil-test-buffer
      "[ひ]らがな1234"
      ("viw")
      "<ひらが[な]>1234")
    (evil-test-buffer
      "[ひ]らがな1234"
      ("vaw")
      "<ひらが[な]>1234")
    (evil-test-buffer
      "[ひ]らがな漢字"
      ("viw")
      "<ひらが[な]>漢字")
    (evil-test-buffer
      "[ひ]らがな漢字"
      ("vaw")
      "<ひらが[な]>漢字")
    (evil-test-buffer
      "[ひ]らがなカタカナ"
      ("viw")
      "<ひらが[な]>カタカナ")
    (evil-test-buffer
      "[ひ]らがなカタカナ"
      ("vaw")
      "<ひらが[な]>カタカナ")
    (evil-test-buffer
      "[ひ]らがなｶﾀｶﾅ"
      ("viw")
      "<ひらが[な]>ｶﾀｶﾅ")
    (evil-test-buffer
      "[ひ]らがなｶﾀｶﾅ"
      ("vaw")
      "<ひらが[な]>ｶﾀｶﾅ")
    (evil-test-buffer
      "[ひ]らがなＡＢＣ"
      ("viw")
      "<ひらが[な]>ＡＢＣ")
    (evil-test-buffer
      "[ひ]らがなＡＢＣ"
      ("vaw")
      "<ひらが[な]>ＡＢＣ")
    (evil-test-buffer
      "[ひ]らがな１２３"
      ("viw")
      "<ひらが[な]>１２３")
    (evil-test-buffer
      "[ひ]らがな１２３"
      ("vaw")
      "<ひらが[な]>１２３")
    (evil-test-buffer
      "[ひ]らがな한글"
      ("viw")
      "<ひらが[な]>한글")
    (evil-test-buffer
      "[ひ]らがな한글"
      ("vaw")
      "<ひらが[な]>한글")
    (evil-test-buffer
      "[カ]タカナabcd"
      ("viw")
      "<カタカ[ナ]>abcd")
    (evil-test-buffer
      "[カ]タカナabcd"
      ("vaw")
      "<カタカ[ナ]>abcd")
    (evil-test-buffer
      "[カ]タカナ1234"
      ("viw")
      "<カタカ[ナ]>1234")
    (evil-test-buffer
      "[カ]タカナ1234"
      ("vaw")
      "<カタカ[ナ]>1234")
    (evil-test-buffer
      "[カ]タカナ漢字"
      ("viw")
      "<カタカ[ナ]>漢字")
    (evil-test-buffer
      "[カ]タカナ漢字"
      ("vaw")
      "<カタカ[ナ]>漢字")
    (evil-test-buffer
      "[カ]タカナひらがな"
      ("viw")
      "<カタカ[ナ]>ひらがな")
    (evil-test-buffer
      "[カ]タカナひらがな"
      ("vaw")
      "<カタカ[ナ]>ひらがな")
    (evil-test-buffer
      "[カ]タカナｶﾀｶﾅ"
      ("viw")
      "<カタカ[ナ]>ｶﾀｶﾅ")
    (evil-test-buffer
      "[カ]タカナｶﾀｶﾅ"
      ("vaw")
      "<カタカ[ナ]>ｶﾀｶﾅ")
    (evil-test-buffer
      "[カ]タカナＡＢＣ"
      ("viw")
      "<カタカ[ナ]>ＡＢＣ")
    (evil-test-buffer
      "[カ]タカナＡＢＣ"
      ("vaw")
      "<カタカ[ナ]>ＡＢＣ")
    (evil-test-buffer
      "[カ]タカナ１２３"
      ("viw")
      "<カタカ[ナ]>１２３")
    (evil-test-buffer
      "[カ]タカナ１２３"
      ("vaw")
      "<カタカ[ナ]>１２３")
    (evil-test-buffer
      "[カ]タカナ한글"
      ("viw")
      "<カタカ[ナ]>한글")
    (evil-test-buffer
      "[カ]タカナ한글"
      ("vaw")
      "<カタカ[ナ]>한글")
    (evil-test-buffer
      "[ｶ]ﾀｶﾅabcd"
      ("viw")
      "<ｶﾀｶﾅabc[d]>")
    (evil-test-buffer
      "[ｶ]ﾀｶﾅabcd"
      ("vaw")
      "<ｶﾀｶﾅabc[d]>")
    (evil-test-buffer
      "[ｶ]ﾀｶﾅ1234"
      ("viw")
      "<ｶﾀｶﾅ123[4]>")
    (evil-test-buffer
      "[ｶ]ﾀｶﾅ1234"
      ("vaw")
      "<ｶﾀｶﾅ123[4]>")
    (evil-test-buffer
      "[ｶ]ﾀｶﾅ漢字"
      ("viw")
      "<ｶﾀｶ[ﾅ]>漢字")
    (evil-test-buffer
      "[ｶ]ﾀｶﾅ漢字"
      ("vaw")
      "<ｶﾀｶ[ﾅ]>漢字")
    (evil-test-buffer
      "[ｶ]ﾀｶﾅひらがな"
      ("viw")
      "<ｶﾀｶ[ﾅ]>ひらがな")
    (evil-test-buffer
      "[ｶ]ﾀｶﾅひらがな"
      ("vaw")
      "<ｶﾀｶ[ﾅ]>ひらがな")
    (evil-test-buffer
      "[ｶ]ﾀｶﾅカタカナ"
      ("viw")
      "<ｶﾀｶ[ﾅ]>カタカナ")
    (evil-test-buffer
      "[ｶ]ﾀｶﾅカタカナ"
      ("vaw")
      "<ｶﾀｶ[ﾅ]>カタカナ")
    (evil-test-buffer
      "[ｶ]ﾀｶﾅＡＢＣ"
      ("viw")
      "<ｶﾀｶﾅＡＢ[Ｃ]>")
    (evil-test-buffer
      "[ｶ]ﾀｶﾅＡＢＣ"
      ("vaw")
      "<ｶﾀｶﾅＡＢ[Ｃ]>")
    (evil-test-buffer
      "[ｶ]ﾀｶﾅ１２３"
      ("viw")
      "<ｶﾀｶﾅ１２[３]>")
    (evil-test-buffer
      "[ｶ]ﾀｶﾅ１２３"
      ("vaw")
      "<ｶﾀｶﾅ１２[３]>")
    (evil-test-buffer
      "[ｶ]ﾀｶﾅ한글"
      ("viw")
      "<ｶﾀｶ[ﾅ]>한글")
    (evil-test-buffer
      "[ｶ]ﾀｶﾅ한글"
      ("vaw")
      "<ｶﾀｶ[ﾅ]>한글")
    (evil-test-buffer
      "[Ａ]ＢＣabcd"
      ("viw")
      "<ＡＢＣabc[d]>")
    (evil-test-buffer
      "[Ａ]ＢＣabcd"
      ("vaw")
      "<ＡＢＣabc[d]>")
    (evil-test-buffer
      "[Ａ]ＢＣ1234"
      ("viw")
      "<ＡＢＣ123[4]>")
    (evil-test-buffer
      "[Ａ]ＢＣ1234"
      ("vaw")
      "<ＡＢＣ123[4]>")
    (evil-test-buffer
      "[Ａ]ＢＣ漢字"
      ("viw")
      "<ＡＢ[Ｃ]>漢字")
    (evil-test-buffer
      "[Ａ]ＢＣ漢字"
      ("vaw")
      "<ＡＢ[Ｃ]>漢字")
    (evil-test-buffer
      "[Ａ]ＢＣひらがな"
      ("viw")
      "<ＡＢ[Ｃ]>ひらがな")
    (evil-test-buffer
      "[Ａ]ＢＣひらがな"
      ("vaw")
      "<ＡＢ[Ｃ]>ひらがな")
    (evil-test-buffer
      "[Ａ]ＢＣカタカナ"
      ("viw")
      "<ＡＢ[Ｃ]>カタカナ")
    (evil-test-buffer
      "[Ａ]ＢＣカタカナ"
      ("vaw")
      "<ＡＢ[Ｃ]>カタカナ")
    (evil-test-buffer
      "[Ａ]ＢＣｶﾀｶﾅ"
      ("viw")
      "<ＡＢＣｶﾀｶ[ﾅ]>")
    (evil-test-buffer
      "[Ａ]ＢＣｶﾀｶﾅ"
      ("vaw")
      "<ＡＢＣｶﾀｶ[ﾅ]>")
    (evil-test-buffer
      "[Ａ]ＢＣ１２３"
      ("viw")
      "<ＡＢＣ１２[３]>")
    (evil-test-buffer
      "[Ａ]ＢＣ１２３"
      ("vaw")
      "<ＡＢＣ１２[３]>")
    (evil-test-buffer
      "[Ａ]ＢＣ한글"
      ("viw")
      "<ＡＢ[Ｃ]>한글")
    (evil-test-buffer
      "[Ａ]ＢＣ한글"
      ("vaw")
      "<ＡＢ[Ｃ]>한글")
    (evil-test-buffer
      "[１]２３abcd"
      ("viw")
      "<１２３abc[d]>")
    (evil-test-buffer
      "[１]２３abcd"
      ("vaw")
      "<１２３abc[d]>")
    (evil-test-buffer
      "[１]２３1234"
      ("viw")
      "<１２３123[4]>")
    (evil-test-buffer
      "[１]２３1234"
      ("vaw")
      "<１２３123[4]>")
    (evil-test-buffer
      "[１]２３漢字"
      ("viw")
      "<１２[３]>漢字")
    (evil-test-buffer
      "[１]２３漢字"
      ("vaw")
      "<１２[３]>漢字")
    (evil-test-buffer
      "[１]２３ひらがな"
      ("viw")
      "<１２[３]>ひらがな")
    (evil-test-buffer
      "[１]２３ひらがな"
      ("vaw")
      "<１２[３]>ひらがな")
    (evil-test-buffer
      "[１]２３カタカナ"
      ("viw")
      "<１２[３]>カタカナ")
    (evil-test-buffer
      "[１]２３カタカナ"
      ("vaw")
      "<１２[３]>カタカナ")
    (evil-test-buffer
      "[１]２３ｶﾀｶﾅ"
      ("viw")
      "<１２３ｶﾀｶ[ﾅ]>")
    (evil-test-buffer
      "[１]２３ｶﾀｶﾅ"
      ("vaw")
      "<１２３ｶﾀｶ[ﾅ]>")
    (evil-test-buffer
      "[１]２３ＡＢＣ"
      ("viw")
      "<１２３ＡＢ[Ｃ]>")
    (evil-test-buffer
      "[１]２３ＡＢＣ"
      ("vaw")
      "<１２３ＡＢ[Ｃ]>")
    (evil-test-buffer
      "[１]２３한글"
      ("viw")
      "<１２[３]>한글")
    (evil-test-buffer
      "[１]２３한글"
      ("vaw")
      "<１２[３]>한글")
    (evil-test-buffer
      "[한]글abcd"
      ("viw")
      "<한[글]>abcd")
    (evil-test-buffer
      "[한]글abcd"
      ("vaw")
      "<한[글]>abcd")
    (evil-test-buffer
      "[한]글1234"
      ("viw")
      "<한[글]>1234")
    (evil-test-buffer
      "[한]글1234"
      ("vaw")
      "<한[글]>1234")
    (evil-test-buffer
      "[한]글漢字"
      ("viw")
      "<한[글]>漢字")
    (evil-test-buffer
      "[한]글漢字"
      ("vaw")
      "<한[글]>漢字")
    (evil-test-buffer
      "[한]글ひらがな"
      ("viw")
      "<한[글]>ひらがな")
    (evil-test-buffer
      "[한]글ひらがな"
      ("vaw")
      "<한[글]>ひらがな")
    (evil-test-buffer
      "[한]글カタカナ"
      ("viw")
      "<한[글]>カタカナ")
    (evil-test-buffer
      "[한]글カタカナ"
      ("vaw")
      "<한[글]>カタカナ")
    (evil-test-buffer
      "[한]글ｶﾀｶﾅ"
      ("viw")
      "<한[글]>ｶﾀｶﾅ")
    (evil-test-buffer
      "[한]글ｶﾀｶﾅ"
      ("vaw")
      "<한[글]>ｶﾀｶﾅ")
    (evil-test-buffer
      "[한]글ＡＢＣ"
      ("viw")
      "<한[글]>ＡＢＣ")
    (evil-test-buffer
      "[한]글ＡＢＣ"
      ("vaw")
      "<한[글]>ＡＢＣ")
    (evil-test-buffer
      "[한]글１２３"
      ("viw")
      "<한[글]>１２３")
    (evil-test-buffer
      "[한]글１２３"
      ("vaw")
      "<한[글]>１２３")))

(ert-deftest evil-test-paragraph-objects ()
  "Test `evil-inner-paragraph' and `evil-a-paragraph'"
  :tags '(evil text-object)
  (ert-info ("Select a paragraph with point at beginning")
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
;; and for Lisp evaluation."))
  (ert-info ("Select a paragraph with point at last line")
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
;; and for Lisp evaluation."))
  (ert-info ("Select a paragraph with point after paragraph")
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
;; and for Lisp evaluation.[]
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
;; and for Lisp evaluation.[]
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
      "This is '<a tes[t]>' for quote objects.")
    (evil-test-buffer
      "This is \"a '[t]est'\" for quote objects."
      ("vi'")
      "This is \"a '<tes[t]>'\" for quote objects."))
  (ert-info ("Select text including enclosing quotes")
    (evil-test-buffer
      "This is 'a [t]est' for quote objects."
      ("v2i'")
      "This is <'a test[']> for quote objects."))
  (ert-info ("Select text including enclosing quotes and following space")
    (evil-test-buffer
      "This is 'a [t]est' for quote objects."
      ("va'")
      "This is <'a test'[ ]>for quote objects."))
  (ert-info ("Select text including enclosing quotes and previous space")
    (evil-test-buffer
      "This is 'a [t]est'. For quote objects."
      ("va'")
      "This is< 'a test[']>. For quote objects."))
  (ert-info ("Select text on opening quote")
    (evil-test-buffer
      "This is [\"]a test\". For \"quote\" objects."
      (emacs-lisp-mode)
      ("va\"")
      "This is< \"a test[\"]>. For \"quote\" objects."))
  (ert-info ("Select text on closing quote")
    (evil-test-buffer
      "This is \"a test[\"]. For \"quote\" objects."
      (emacs-lisp-mode)
      ("va\"")
      "This is< \"a test[\"]>. For \"quote\" objects."))
  (ert-info ("Delete text from outside")
    (evil-test-buffer
      "Th[i]s is \"a test\". For \"quote\" objects."
      (emacs-lisp-mode)
      ("da\"")
      "This is[.] For \"quote\" objects."))
  (ert-info ("Operator on empty quotes")
    (evil-test-buffer
      "This is [a]n \"\" empty quote"
      (emacs-lisp-mode)
      ("ci\"XXX" [escape])
      "This is an \"XX[X]\" empty quote")))

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
  (ert-info ("Select double inner parentheses")
    (evil-test-buffer
      "([(]word))"
      ("dib")
      "(())")
    (evil-test-buffer
      "[(](word))"
      ("dib")
      "()")
    (evil-test-buffer
      "((word[)])"
      ("dib")
      "(())")
    (evil-test-buffer
      "((word)[)]"
      ("dib")
      "()"))
  (ert-info ("Select double outer parentheses")
    (evil-test-buffer
      "a([(]word))b"
      ("dab")
      "a()b")
    (evil-test-buffer
      "a[(](word))b"
      ("dab")
      "ab")
    (evil-test-buffer
      "a((word[)])b"
      ("dab")
      "a()b")
    (evil-test-buffer
      "a((word)[)]b"
      ("dab")
      "ab"))
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
      "<(aaa \"bbbb\" aa[)]>"))
  (ert-info ("Select inner parentheses around strings")
    (evil-test-buffer
      "((\"t[e]st\"))\n"
      (emacs-lisp-mode)
      ("vib")
      "((<\"test[\"]>))\n"
      ("ib")
      "(<(\"test\"[)]>)\n")
    (evil-test-buffer
      "( ( \"t[e]st\" ) )\n"
      (emacs-lisp-mode)
      ("vib")
      "( (< \"test\"[ ]>) )\n"
      ("ib")
      "(< ( \"test\" )[ ]>)\n")
    (evil-test-buffer
      "((\"t[e]st\"))\n"
      (emacs-lisp-mode)
      ("vhhib")
      "((<[\"]test\">))\n"
      ("ib")
      "(<[(]\"test\")>)\n")
    (evil-test-buffer
      "( ( \"t[e]st\" ) )\n"
      (emacs-lisp-mode)
      ("vhhib")
      "( (<[ ]\"test\" >) )\n"
      ("ib")
      "(<[ ]( \"test\" ) >)\n"))
  (ert-info ("Select outer parentheses around strings")
    (evil-test-buffer
      "((\"t[e]st\"))\n"
      (emacs-lisp-mode)
      ("vab")
      "(<(\"test\"[)]>)\n"
      ("ab")
      "<((\"test\")[)]>\n")
    (evil-test-buffer
      "( ( \"t[e]st\" ) )\n"
      (emacs-lisp-mode)
      ("vab")
      "( <( \"test\" [)]> )\n"
      ("ab")
      "<( ( \"test\" ) [)]>\n")
    (evil-test-buffer
      "((\"t[e]st\"))\n"
      (emacs-lisp-mode)
      ("vhhab")
      "(<[(]\"test\")>)\n"
      ("ab")
      "<[(](\"test\"))>\n")
    (evil-test-buffer
      "( ( \"t[e]st\" ) )\n"
      (emacs-lisp-mode)
      ("vhhab")
      "( <[(] \"test\" )> )\n"
      ("ab")
      "<[(] ( \"test\" ) )>\n")
    (evil-test-buffer
      "(([\"]\"))\n"
      ("dab")
      "([)]\n"))
  (ert-info ("Select inner paren on different lines")
    (evil-test-buffer
      "for (auto i : vector) {
  if (cond) {
    do_[s]omething();
  }
}"
      ("vi}")
      "for (auto i : vector) {
  if (cond) {
<    do_something();[\n]>  }\n}"
      ("i}")
      "for (auto i : vector) {
<  if (cond) {
    do_something();
  }[\n]>}"))
  (ert-info ("Enlarge to smallest complete surrounding")
    (evil-test-buffer
      "for (auto i : vector) {
  if (c<ond) {
    do_[s]>omething();
  }
}"
      ("i}")
      "for (auto i : vector) {
<  if (cond) {
    do_something();
  }[\n]>}"))
  (ert-info ("yank on blocks is turned linewise")
    (evil-test-buffer
      "{\n  [f]oo();\n}\n"
      ("yiBp")
      "{\n  foo();\n  [f]oo();\n}\n"))
  (ert-info ("exclusive like if ending at bol")
    (evil-test-buffer
      "(defun foo ()\n[ ] (insert \"bar\")\n  )\n"
      ("cibx" [escape])
      "([x]\n  )\n"))
  (ert-info ("Operator on empty parentheses")
    (evil-test-buffer
      "a([(]))b"
      ("cibx" [escape])
      "a(([x]))b")
    (evil-test-buffer
      "a(([)])b"
      ("cibx" [escape])
      "a(([x]))b")))

(ert-deftest evil-test-forces-linewise-text-objects ()
  "Test `evil-text-object-change-visual-type' option."
  :tags '(evil text-object)
  (let ((evil-text-object-change-visual-type t))
    (ert-info ("Change visual type")
      (evil-test-buffer
        "  function(opts) {
    this.var1 = something();
    [t]his.var2 = something_else();
    return something_nasty();
  }
"
        ("Vi}")
        "  function(opts) {
<    this.var1 = something();
    this.var2 = something_else();
    return something_nasty();[
]>  }
"
        (should (eq (evil-visual-type) 'inclusive)))))
  (let ((evil-text-object-change-visual-type nil))
    (ert-info ("Change visual type keeping linewise")
      (evil-test-buffer
        "  function(opts) {
    this.var1 = something();
    [t]his.var2 = something_else();
    return something_nasty();
  }
"
        ("Vi}")
        "  function(opts) {
<    this.var1 = something();
    this.var2 = something_else();
    return something_nasty();\n>  }
"
        (should (eq (evil-visual-type) 'line)))))
  (let ((evil-text-object-change-visual-type nil))
    (ert-info ("Linewise outer block")
      (evil-test-buffer
        "  function(opts) {
    this.var1 = something();
    [t]his.var2 = something_else();
    return something_nasty();
  }
"
        ("Va}")
        "<  function(opts) {
    this.var1 = something();
    this.var2 = something_else();
    return something_nasty();
  }
>"
        (should (eq (evil-visual-type) 'line)))))
  (ert-info ("Forced motion type should change text object type")
    (evil-test-buffer
      "for (int i=0; i<10; i++) {
  if ([c]ond) {
    do_something();
  }
}"
      ("dVi}")
      "for (int i=0; i<10; i++) {
\[}]")))

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
      "{<aaa>bbbb</aaa[>]}"))
  (ert-info ("Handle quoted strings tags")
    (evil-test-buffer
      :visual-start "{"
      :visual-end "}"
      "<html>
<body>
<div id=\"content\">
\[ ]
<p>
UPDATE
</p>
<p>
test hello <a href=\"/deed.zh\">Creative Commons</a>
</p>
</div>
</body>
</html>
"
      ("vit")
      "<html>
<body>
<div id=\"content\">{\n \n<p>
UPDATE
</p>
<p>
test hello <a href=\"/deed.zh\">Creative Commons</a>
</p>[\n]}</div>
</body>
</html>
"

      )))

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
;; and for Lisp evaluation.")
  (ert-info ("Test `evil-want-visual-char-semi-exclusive")
    (let ((evil-want-visual-char-semi-exclusive t))
      (evil-test-buffer
        "[;]; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.
;; And a third line."
        ("v")
        "<[;]>; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.
;; And a third line."
        ("$")
        "<;; This buffer is for notes you don't want to save,>[
];; and for Lisp evaluation.
;; And a third line."
        ("^jj")
        "<;; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.\n>[;]; And a third line."))))

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

;;; Replace state

(ert-deftest evil-test-replacement ()
  "Test replacing consecutive characters"
  :tags '(evil replace)
  (ert-info ("Replace and restore consecutive characters")
    (evil-test-buffer
     ";; [T]his buffer is for notes"
     ("Rfoo")
     ";; foo[s] buffer is for notes"
     ([backspace backspace backspace])
     ";; [T]his buffer is for notes"))
  (ert-info ("Replace and restore consecutive characters beyond eol")
    (evil-test-buffer
     ";; [T]his buffer is for notes"
     ("wwwwRxxxxxxx")
     ";; This buffer is for xxxxxxx[]"
     ([backspace backspace backspace backspace backspace backspace backspace])
     ";; This buffer is for [n]otes"))
  (ert-info ("Replace from line below and restore")
    (define-key evil-replace-state-map (kbd "C-e") 'evil-copy-from-below)
    (evil-test-buffer
     ";; [f]oo bar\n;; qux quux"
     ("R\C-e\C-e\C-e")
     ";; qux[ ]bar\n;; qux quux"
     ([backspace backspace backspace])
     ";; [f]oo bar\n;; qux quux")
    (define-key evil-replace-state-map (kbd "C-e") nil))
  (ert-info ("Replace from line above and restore")
    (define-key evil-replace-state-map (kbd "C-y") 'evil-copy-from-above)
    (evil-test-buffer
     ";; foo bar\n;; [q]ux quux"
     ("R\C-y\C-y\C-y")
     ";; foo bar\n;; foo[ ]quux"
     ([backspace backspace backspace])
     ";; foo bar\n;; [q]ux quux")
    (define-key evil-replace-state-map (kbd "C-y") nil)))

;;; Ex

(ert-deftest evil-test-ex-parse ()
  "Test `evil-ex-parse'"
  :tags '(evil ex)
  (should (equal (evil-ex-parse "5,2cmd arg")
                 '(evil-ex-call-command
                   (evil-ex-range
                    (evil-ex-line (string-to-number "5") nil)
                    (evil-ex-line (string-to-number "2") nil))
                   "cmd"
                   "arg")))
  (should (equal (evil-ex-parse "5,2cmd !arg")
                 '(evil-ex-call-command
                   (evil-ex-range
                    (evil-ex-line (string-to-number "5") nil)
                    (evil-ex-line (string-to-number "2") nil))
                   "cmd"
                   "!arg")))
  (should (equal (evil-ex-parse "5,2 arg")
                 '(evil-ex-call-command
                   (evil-ex-range
                    (evil-ex-line (string-to-number "5") nil)
                    (evil-ex-line (string-to-number "2") nil))
                   "arg"
                   nil))))

(ert-deftest evil-test-ex-parse-ranges ()
  "Test parsing of ranges"
  :tags '(evil ex)
  (should (equal (evil-ex-parse "%" nil 'range)
                 '(evil-ex-full-range)))
  (should (equal (evil-ex-parse "5,27" nil 'range)
                 '(evil-ex-range
                   (evil-ex-line (string-to-number "5") nil)
                   (evil-ex-line (string-to-number "27") nil))))
  (should (equal (evil-ex-parse "5,$" nil 'range)
                 '(evil-ex-range
                   (evil-ex-line (string-to-number "5") nil)
                   (evil-ex-line (evil-ex-last-line) nil))))
  (should (equal (evil-ex-parse "5,'x" nil 'range)
                 '(evil-ex-range
                   (evil-ex-line (string-to-number "5") nil)
                   (evil-ex-line (evil-ex-marker "x") nil))))
  (should (equal (evil-ex-parse "`x,`y" nil 'range)
                 '(evil-ex-char-marker-range "x" "y")))
  (should (equal (evil-ex-parse "5,+" nil 'range)
                 '(evil-ex-range
                   (evil-ex-line (string-to-number "5") nil)
                   (evil-ex-line
                    nil (+ (evil-ex-signed-number (intern "+") nil))))))
  (should (equal (evil-ex-parse "5,-" nil 'range)
                 '(evil-ex-range
                   (evil-ex-line (string-to-number "5") nil)
                   (evil-ex-line
                    nil (+ (evil-ex-signed-number (intern "-") nil))))))
  (should (equal (evil-ex-parse "5,4+2-7-3+10-" nil 'range)
                 '(evil-ex-range
                   (evil-ex-line (string-to-number "5") nil)
                   (evil-ex-line
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
  (should (equal (evil-ex-parse ".-2,4+2-7-3+10-" nil 'range)
                 '(evil-ex-range
                   (evil-ex-line
                    (evil-ex-current-line)
                    (+ (evil-ex-signed-number
                        (intern "-") (string-to-number "2"))))
                   (evil-ex-line
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
                   (evil-ex-line
                    (evil-ex-marker "a")
                    (+ (evil-ex-signed-number
                        (intern "-") (string-to-number "2"))))
                   (evil-ex-line
                    (evil-ex-last-line)
                    (+ (evil-ex-signed-number
                        (intern "-") (string-to-number "10")))))))
  (should (equal (evil-ex-parse ".+42" nil 'range)
                 '(evil-ex-range
                   (evil-ex-line
                    (evil-ex-current-line)
                    (+ (evil-ex-signed-number
                        (intern "+") (string-to-number "42"))))
                   nil))))

(ert-deftest evil-test-ex-parse-emacs-commands ()
  "Test parsing of Emacs commands"
  :tags '(evil ex)
  (should (equal (evil-ex-parse "ido-mode")
                 '(evil-ex-call-command nil "ido-mode" nil)))
  (should (equal (evil-ex-parse "yas/reload-all")
                 '(evil-ex-call-command nil "yas/reload-all" nil)))
  (should (equal (evil-ex-parse "mu4e")
                 '(evil-ex-call-command nil "mu4e" nil))))

(ert-deftest evil-text-ex-search-offset ()
  "Test for addresses like /base//pattern/"
  :tags '(evil ex)
  (ert-info ("without base")
    (evil-test-buffer
      "[l]ine 1\naaa\nbbb\naaa\nccc\nddd"
      (":/aaa/d")
      "line 1\nbbb\naaa\nccc\nddd"))
  (ert-info ("with base")
    (evil-test-buffer
      "[l]ine 1\naaa\nbbb\naaa\nccc\nddd"
      (":/bbb//aaa/d")
      "line 1\naaa\nbbb\nccc\nddd"))
  (ert-info ("range without base")
    (evil-test-buffer
      "[l]ine 1\naaa\nbbb\naaa\nccc\nddd\nccc\neee\n"
      (":/aaa/;/ccc/d")
      "line 1\nddd\nccc\neee\n"))
  (ert-info ("range with base")
    (evil-test-buffer
      "[l]ine 1\naaa\nbbb\naaa\nccc\nddd\nccc\neee\n"
      (":/bbb//aaa/;/ddd//ccc/d")
      "line 1\naaa\nbbb\neee\n")))

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

(ert-deftest evil-test-ex-repeat2 ()
  "Test @: command."
  :tags '(evil ex)
  (evil-without-display
    (ert-info ("Repeat in current line")
      (evil-test-buffer
        "[a]bcdef\nabcdef\nabcdef"
        (":s/[be]/X" [return])
        "[a]Xcdef\nabcdef\nabcdef"
        ("jj@:")
        "aXcdef\nabcdef\n[a]Xcdef"))
    (ert-info ("Repeat with count in current line")
      (evil-test-buffer
        "[a]bcdef\nabcdef\nabcdef"
        (":s/[be]/X" [return])
        "[a]Xcdef\nabcdef\nabcdef"
        ("jj2@:")
        "aXcdef\nabcdef\n[a]XcdXf"))
    (ert-info ("Do not record dot repeat")
      (evil-test-buffer
        ""
        ("OAAAAAA" [escape] "^")
        "[A]AAAAA\n"
        (":s/A/X" [return])
        "[X]AAAAA\n"
        ("@:")
        "[X]XAAAA\n"
        (".")
        "AAAAAA\nXXAAAA\n"))))

(ert-deftest evil-test-ex-visual-char-range ()
  "Test visual character ranges in ex state."
  :tags '(evil ex visual)
  (evil-without-display
    (ert-info ("No character range, inclusive")
      (let ((evil-visual-char 'inclusive)
            evil-ex-visual-char-range)
        (evil-test-buffer
          "li[n]e 1\nline 2\nline 3\nline 4\n"
          ("vjll:d" [return])
          "line 3\nline 4\n")))
    (ert-info ("No character range, exclusive")
      (let ((evil-visual-char 'inclusive)
            evil-ex-visual-char-range)
        (evil-test-buffer
          "li[n]e 1\nline 2\nline 3\nline 4\n"
          ("vjll:d" [return])
          "line 3\nline 4\n")))
    (ert-info ("Character range, inclusive")
      (let ((evil-visual-char 'inclusive)
            (evil-ex-visual-char-range t))
        (evil-test-buffer
          "li[n]e 1\nline 2\nline 3\nline 4\n"
          ("vjll:d" [return])
          "li2\nline 3\nline 4\n")))
    (ert-info ("Character range, exclusive")
      (let ((evil-visual-char 'exclusive)
            (evil-ex-visual-char-range t))
        (evil-test-buffer
          "li[n]e 1\nline 2\nline 3\nline 4\n"
          ("vjll:d" [return])
          "li 2\nline 3\nline 4\n")))))

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
      "[x]xx bar_FOO foo BAR bar_FOO"))
  (ert-info ("Substitute with escaped characters in replacement")
    (evil-test-buffer
      "[a]bcXdefXghiXjkl\n"
      (":s/X/\\|\\/\\|/g" [return])
      "[a]bc|/|def|/|ghi|/|jkl\n"))
  (ert-info ("Substitute with register")
    (evil-test-buffer
      "[a]bc\niiiXiiiXiiiXiii\n"
      ("\"ayiwj:s/X/\\=@a/g" [return])
      "abc\n[i]iiabciiiabciiiabciii\n"))
  (ert-info ("Substitute newlines")
    (evil-test-buffer
      "[a]bc\ndef\nghi\n"
      (":%s/\n/z/" [return])
      "[a]bczdefzghiz"))
  (ert-info ("Substitute newlines with g flag")
    (evil-test-buffer
      "[a]bc\ndef\nghi\n"
      (":%s/\n/z/g" [return])
      "[a]bczdefzghiz"))
  (ert-info ("Substitute newlines without newline in regexp")
    (evil-test-buffer
      "[A]BC\nDEF\nGHI\n"
      (":%s/[^]]*/z/" [return])
      "Z"))
  (ert-info ("Substitute n flag does not replace")
    (evil-test-buffer
      "[a]bc\naef\nahi\n"
      (":%s/a//n" [return])
      "[a]bc\naef\nahi\n"))
  (ert-info ("Substitute n flag does not replace with g flag")
    (evil-test-buffer
      "[a]bc\naef\nahi\n"
      (":%s/a//gn" [return])
      "[a]bc\naef\nahi\n"))
  (ert-info ("Substitute $ does not loop infinitely")
    (evil-test-buffer
      "[a]bc\ndef\nghi"
      (":%s/$/ END/g" [return])
      "abc END\ndef END\n[g]hi END"))
  (ert-info ("Substitute the zero-length beginning of line character")
    (evil-test-buffer
      "[a]bc\ndef\nghi"
      (":s/^/ #/" [return])
      " [#]abc\ndef\nghi"))
  (ert-info ("Substitute the zero-length beginning of line character with g flag")
    (evil-test-buffer
      "[a]bc\ndef\nghi"
      (":s/^/ #/g" [return])
      " [#]abc\ndef\nghi"))
  (ert-info ("Use Substitute to delete individual characters")
    (evil-test-buffer
      "[x]xyxxz"
      (":%s/x//g" [return])
      "[y]z")))

(ert-deftest evil-test-ex-repeat-substitute-replacement ()
  "Test `evil-ex-substitute' with repeating of previous substitutions."
  :tags '(evil ex search)
  (ert-info ("Repeat previous pattern")
    (evil-select-search-module 'evil-search-module 'evil-search)
    (evil-test-buffer
      "[x]xx foo bar foo bar foo bar"
      (":s/foo/AAA" [return])
      "[x]xx AAA bar foo bar foo bar"
      (":s//BBB" [return])
      "[x]xx AAA bar BBB bar foo bar"
      ("/bar" [return] ":s//CCC" [return])
      "[x]xx AAA CCC BBB bar foo bar"
      (":s/ar/XX" [return])
      "[x]xx AAA CCC BBB bXX foo bar"
      (":s//YY" [return])
      "[x]xx AAA CCC BBB bXX foo bYY"))
  (ert-info ("Repeat previous replacement")
    (evil-test-buffer
      "[x]xx foo bar foo bar foo bar"
      (":s/foo/AAA" [return])
      "[x]xx AAA bar foo bar foo bar"
      (":s/bar/~" [return])
      "[x]xx AAA AAA foo bar foo bar"))
  (ert-info ("Repeat with previous flags")
    (evil-test-buffer
      "[x]xx foo bar foo bar foo bar"
      (":s/foo/AAA/g" [return])
      "[x]xx AAA bar AAA bar AAA bar"
      (":s/bar/BBB/&" [return])
      "[x]xx AAA BBB AAA BBB AAA BBB"))
  (ert-info ("Repeat previous substitute without flags")
    (evil-select-search-module 'evil-search-module 'evil-search)
    (evil-test-buffer
      "[x]xx foo bar foo bar foo bar\nxxx foo bar foo bar foo bar"
      (":s/foo/AAA/g" [return])
      "[x]xx AAA bar AAA bar AAA bar\nxxx foo bar foo bar foo bar"
      ("j:s" [return])
      "xxx AAA bar AAA bar AAA bar\n[x]xx AAA bar foo bar foo bar"
      ("/bar" [return] ":s" [return])
      "xxx AAA bar AAA bar AAA bar\n[x]xx AAA bar AAA bar foo bar")
    (evil-test-buffer
      "[x]xx foo bar foo bar foo bar\nxxx foo bar foo bar foo bar"
      (":s/foo/AAA/g" [return])
      "[x]xx AAA bar AAA bar AAA bar\nxxx foo bar foo bar foo bar"
      ("j&")
      "xxx AAA bar AAA bar AAA bar\n[x]xx AAA bar foo bar foo bar")
    (evil-test-buffer
      "[x]xx foo bar foo bar foo bar\nxxx foo bar foo bar foo bar"
      (":s/foo/AAA/g" [return])
      "[x]xx AAA bar AAA bar AAA bar\nxxx foo bar foo bar foo bar"
      ("j:&" [return])
      "xxx AAA bar AAA bar AAA bar\n[x]xx AAA bar foo bar foo bar"))
  (ert-info ("Repeat previous substitute with the same flags")
    (evil-test-buffer
      "[x]xx foo bar foo bar foo bar\nxxx foo bar foo bar foo bar"
      (":s/foo/AAA/g" [return])
      "[x]xx AAA bar AAA bar AAA bar\nxxx foo bar foo bar foo bar"
      ("j:s//~/&" [return])
      "xxx AAA bar AAA bar AAA bar\n[x]xx AAA bar AAA bar AAA bar")
    (evil-test-buffer
      "[x]xx foo bar foo bar foo bar\nxxx foo bar foo bar foo bar"
      (":s/foo/AAA/g" [return])
      "[x]xx AAA bar AAA bar AAA bar\nxxx foo bar foo bar foo bar"
      ("j:&&" [return])
      "xxx AAA bar AAA bar AAA bar\n[x]xx AAA bar AAA bar AAA bar"))
  (ert-info ("Repeat previous substitute with new flags")
    (evil-test-buffer
      "[x]xx foo bar foo bar foo bar\nxxx foo bar foo bar foo bar"
      (":s/foo/AAA" [return])
      "[x]xx AAA bar foo bar foo bar\nxxx foo bar foo bar foo bar"
      ("j:s g" [return])
      "xxx AAA bar foo bar foo bar\n[x]xx AAA bar AAA bar AAA bar")
    (evil-test-buffer
      "[x]xx foo bar foo bar foo bar\nxxx foo bar foo bar foo bar"
      (":s/foo/AAA" [return])
      "[x]xx AAA bar foo bar foo bar\nxxx foo bar foo bar foo bar"
      ("j:& g" [return])
      "xxx AAA bar foo bar foo bar\n[x]xx AAA bar AAA bar AAA bar"))
  (ert-info ("Repeat with previous search pattern")
    (evil-select-search-module 'evil-search-module 'evil-search)
    (evil-test-buffer
      "[x]xx foo bar foo bar foo bar\nxxx foo bar foo bar foo bar"
      (":s/foo/AAA" [return])
      "[x]xx AAA bar foo bar foo bar\nxxx foo bar foo bar foo bar"
      ("/bar" [return])
      "xxx AAA [b]ar foo bar foo bar\nxxx foo bar foo bar foo bar"
      (":2s rg" [return])
      "xxx AAA bar foo bar foo bar\n[x]xx foo AAA foo AAA foo AAA")
    (evil-test-buffer
      "[x]xx foo bar foo bar foo bar\nxxx foo bar foo bar foo bar"
      (":s/foo/AAA" [return])
      "[x]xx AAA bar foo bar foo bar\nxxx foo bar foo bar foo bar"
      ("/bar" [return])
      "xxx AAA [b]ar foo bar foo bar\nxxx foo bar foo bar foo bar"
      (":2~ g" [return])
      "xxx AAA bar foo bar foo bar\n[x]xx foo AAA foo AAA foo AAA"))
  (ert-info ("Repeat previous substitute globally")
    (evil-test-buffer
      "[x]xx foo bar foo bar foo bar\nxxx foo bar foo bar foo bar"
      (":s/foo/AAA/g" [return])
      "[x]xx AAA bar AAA bar AAA bar\nxxx foo bar foo bar foo bar"
      ("g&")
      "xxx AAA bar AAA bar AAA bar\n[x]xx AAA bar AAA bar AAA bar")))

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
        "start you YOU You [y]ou YOU You"))
    (ert-info ("Test failing search does not move point")
      (evil-test-buffer
        "foo [f]oo foo\nbar bar2 bar\nbaz baz baz\n"
        (error search-failed "/foofoo" [return])
        "foo [f]oo foo\nbar bar2 bar\nbaz baz baz\n"
        ("/bar2" [return])
        "foo foo foo\nbar [b]ar2 bar\nbaz baz baz\n"
        ("dw")
        "foo foo foo\nbar [b]ar\nbaz baz baz\n"
        (error search-failed "n")
        "foo foo foo\nbar [b]ar\nbaz baz baz\n"
        (error search-failed "N")
        "foo foo foo\nbar [b]ar\nbaz baz baz\n"))
    (ert-info ("Test search for newline")
      (evil-test-buffer
        "[s]tart\nline 2\nline 3\n\n"
        ("/\\n" [return])
        "star[t]\nline 2\nline 3\n\n"
        ("n")
        "start\nline [2]\nline 3\n\n"
        ("n")
        "start\nline 2\nline [3]\n\n"
        ("n")
        "start\nline 2\nline 3\n[]\n"))))

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
        "foo foo\nbar bar\nbaz baz\n[A]nother line\nAnd yet another line"))
    (ert-info ("Test search next after /$")
      (evil-test-buffer
        "[l]ine 1\nline 2\n\n\line 4\n"
        ("/$" [return])
        "line [1]\nline 2\n\n\line 4\n"
        ("n")
        "line 1\nline [2]\n\n\line 4\n"
        ("n")
        "line 1\nline 2\n[\n]\line 4\n"
        ("n")
        "line 1\nline 2\n\n\line [4]\n"))))

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

(ert-deftest evil-test-ex-search-word ()
  "Test search for word under point."
  :tags '(evil ex search)
  (evil-without-display
    (evil-select-search-module 'evil-search-module 'evil-search)
    (setq evil-ex-search-history nil)
    (evil-test-buffer
      "so[m]e text with a strange word
and here some other stuff
maybe we need one line more with some text\n"
      (setq evil-symbol-word-search nil)
      ("*")
      "some text with a strange word
and here [s]ome other stuff
maybe we need one line more with some text\n"
      ("n")
      "some text with a strange word
and here some other stuff
maybe we need one line more with [s]ome text\n"
      (ert-info ("Search history")
        (should (equal evil-ex-search-history '("\\<some\\>"))))
      ("*")
      "[s]ome text with a strange word
and here some other stuff
maybe we need one line more with some text\n"
      (ert-info ("Search history with double pattern")
        (should (equal evil-ex-search-history '("\\<some\\>")))))
    (ert-info ("Test unbounded search")
      (evil-select-search-module 'evil-search-module 'evil-search)
      (setq evil-ex-search-history nil)
      (evil-test-buffer
        "[s]ymbol\n(defun my-symbolfunc ())\n(defvar my-symbolvar)\nanother symbol\n"
        ("*")
        (setq evil-symbol-word-search nil)
        "symbol\n(defun my-symbolfunc ())\n(defvar my-symbolvar)\nanother [s]ymbol\n"
        ("ggg*")
        "symbol\n(defun my-[s]ymbolfunc ())\n(defvar my-symbolvar)\nanother symbol\n"
        (should (equal evil-ex-search-history '("symbol" "\\<symbol\\>")))
        ("n")
        "symbol\n(defun my-symbolfunc ())\n(defvar my-[s]ymbolvar)\nanother symbol\n"))
    (ert-info ("Test symbol search")
      (evil-select-search-module 'evil-search-module 'evil-search)
      (evil-test-buffer
        "(defun my-s[y]mbol-func ())\n(defvar my-symbol-var)\n(my-symbol-func)\n(setq my-symbol-func2 (my-symbol-func))\n"
        (setq evil-symbol-word-search t)
        ("*")
        "(defun my-symbol-func ())\n(defvar my-symbol-var)\n([m]y-symbol-func)\n(setq my-symbol-func2 (my-symbol-func))\n"
        ("n")
        "(defun my-symbol-func ())\n(defvar my-symbol-var)\n(my-symbol-func)\n(setq my-symbol-func2 ([m]y-symbol-func))\n"))))

(ert-deftest evil-test-isearch-word ()
  "Test isearch for word under point."
  :tags '(evil isearch)
  (evil-without-display
    (evil-select-search-module 'evil-search-module 'isearch)
    (evil-test-buffer
      "so[m]e text with a strange word
and here some other stuff
maybe we need one line more with some text\n"
      (setq evil-symbol-word-search nil)
      ("*")
      "some text with a strange word
and here [s]ome other stuff
maybe we need one line more with some text\n"
      ("n")
      "some text with a strange word
and here some other stuff
maybe we need one line more with [s]ome text\n"
      ("*")
      "[s]ome text with a strange word
and here some other stuff
maybe we need one line more with some text\n")
    (ert-info ("Test unbounded search")
      (evil-select-search-module 'evil-search-module 'isearch)
      (evil-test-buffer
        "[s]ymbol\n(defun my-symbolfunc ())\n(defvar my-symbolvar)\nanother symbol\n"
        (setq evil-symbol-word-search nil)
        ("*")
        "symbol\n(defun my-symbolfunc ())\n(defvar my-symbolvar)\nanother [s]ymbol\n"
        ("ggg*")
        "symbol\n(defun my-[s]ymbolfunc ())\n(defvar my-symbolvar)\nanother symbol\n"
        ("n")
        "symbol\n(defun my-symbolfunc ())\n(defvar my-[s]ymbolvar)\nanother symbol\n"))
    (ert-info ("Test symbol search")
      (evil-select-search-module 'evil-search-module 'isearch)
      (evil-test-buffer
        "(defun my-s[y]mbol-func ())\n(defvar my-symbol-var)\n(my-symbol-func)\n(setq my-symbol-func2 (my-symbol-func))\n"
        (setq evil-symbol-word-search t)
        ("*")
        "(defun my-symbol-func ())\n(defvar my-symbol-var)\n([m]y-symbol-func)\n(setq my-symbol-func2 (my-symbol-func))\n"
        ("n")
        "(defun my-symbol-func ())\n(defvar my-symbol-var)\n(my-symbol-func)\n(setq my-symbol-func2 ([m]y-symbol-func))\n"))))

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

(ert-deftest evil-test-shell-command ()
  "Test `evil-shell-command'."
  (ert-info ("ex shell command")
    (evil-test-buffer
      "[l]ine 5\nline 4\nline 3\nline 2\nline 1\n"
      (":2,3!sort" [return])
      "line 5\n[l]ine 3\nline 4\nline 2\nline 1\n"))
  (ert-info ("shell command operator with count")
    (evil-test-buffer
      "line 5\n[l]ine 4\nline 3\nline 2\nline 1\n"
      ("2!!sort" [return])
      "line 5\n[l]ine 3\nline 4\nline 2\nline 1\n"))
  (ert-info ("shell command operator with motion")
    (evil-test-buffer
      "line 5\n[l]ine 4\nline 3\nline 2\nline 1\n"
      ("!jsort" [return])
      "line 5\n[l]ine 3\nline 4\nline 2\nline 1\n"))
  (ert-info ("shell command operator with backward motion")
    (evil-test-buffer
      "line 5\nline 4\n[l]ine 3\nline 2\nline 1\n"
      ("!ksort" [return])
      "line 5\n[l]ine 3\nline 4\nline 2\nline 1\n"))
  (ert-info ("shell command operator with visual selection")
    (evil-test-buffer
      "line 5\n[l]ine 4\nline 3\nline 2\nline 1\n"
      ("vj!sort" [return])
      "line 5\n[l]ine 3\nline 4\nline 2\nline 1\n")))

(ert-deftest evil-test-global ()
  "Test `evil-ex-global'."
  :tags '(evil ex global)
  (ert-info ("global delete")
    (evil-test-buffer
      "[n]o 1\nno 2\nno 3\nyes 4\nno 5\nno 6\nno 7\n"
      (":g/yes/d" [return])
      "no 1\nno 2\nno 3\n[n]o 5\nno 6\nno 7\n"))
  (ert-info ("global substitute")
    (evil-test-buffer
      "[n]o 1\nno 2\nno 3\nyes 4\nno 5\nno 6\nno 7\n"
      (":g/no/s/[3-6]/x" [return])
      "no 1\nno 2\nno x\nyes 4\nno x\nno x\n[n]o 7\n"
      ("u")
     "no 1\nno 2\nno [3]\nyes 4\nno 5\nno 6\nno 7\n"))
  (evil-select-search-module 'evil-search-module 'evil-search)
  (ert-info ("global use last match if none given, with evil-search")
    (evil-test-buffer
      "[n]o 1\nno 2\nno 3\nyes 4\nno 5\nno 6\nno 7\n"
      ("/yes" [return])
      "no 1\nno 2\nno 3\nyes 4\nno 5\nno 6\nno 7\n"
      (":g//d" [return])
      "no 1\nno 2\nno 3\n[n]o 5\nno 6\nno 7\n"
      (":v//d" [return])
      ""))
  (evil-select-search-module 'evil-search-module 'isearch)
  (ert-info ("global use last match if none given, with isearch")
    (evil-test-buffer
     "[n]o 1\nno 2\nno 3\nisearch 4\nno 5\nno 6\nno 7\n"
     ("/isearch" [return])
     "no 1\nno 2\nno 3\nisearch 4\nno 5\nno 6\nno 7\n"
     (":g//d" [return])
     "no 1\nno 2\nno 3\n[n]o 5\nno 6\nno 7\n"
     (":v//d" [return])
     ""))
  (ert-info (":global should take into account evil-ex-search-case")
    (evil-with-both-search-modules
     (let ((evil-ex-search-case 'sensitive))
       (evil-test-buffer
        "this\nThis\n"
        (":g/this/d" [return])
        "This\n"))
     (let ((evil-ex-search-case 'insensitive))
       (evil-test-buffer
        "this\nThis\n"
        (":g/this/d" [return])
        ""))
     (let ((evil-ex-search-case 'smart))
       (evil-test-buffer
        "this\nThis\n"
        (":g/this/d" [return])
        "")
       (evil-test-buffer
        "this\nThis\n"
        (":g/This/d" [return])
        "this\n")))))

(ert-deftest evil-test-normal ()
  "Test `evil-ex-normal'."
  :tags '(evil ex)
  (let (evil-want-fine-undo)
    (evil-test-buffer
      "[l]ine 1\nline 2\nline 3\nline 4\nline 5\n"
      (":normal lxIABC" [escape] "AXYZ" [return])
      "ABClne 1XY[Z]\nline 2\nline 3\nline 4\nline 5\n"
      (":3,4normal lxIABC" [escape] "AXYZ" [return])
      "ABClne 1XYZ\nline 2\nABClne 3XYZ\nABClne 4XY[Z]\nline 5\n"
      ("u")
      "ABClne 1XYZ\nline 2\nl[i]ne 3\nline 4\nline 5\n")))

(ert-deftest evil-test-copy ()
  :tags '(evil ex)
  "Test `evil-copy'."
  (ert-info ("Copy to last line")
    (evil-test-buffer
      "[l]ine1\nline2\nline3\nline4\nline5\n"
      (":2,3copy$")
      "line1\nline2\nline3\nline4\nline5\nline2\n[l]ine3\n"))
  (ert-info ("Copy to last incomplete line")
    (evil-test-buffer
      "[l]ine1\nline2\nline3\nline4\nline5"
      (":2,3copy$")
      "line1\nline2\nline3\nline4\nline5\nline2\n[l]ine3\n"))
  (ert-info ("Copy incomplete line to last incomplete line")
    (evil-test-buffer
      "[l]ine1\nline2\nline3\nline4\nline5"
      (":4,5copy$")
      "line1\nline2\nline3\nline4\nline5\nline4\n[l]ine5\n"))
  (ert-info ("Copy to first line")
    (evil-test-buffer
      "[l]ine1\nline2\nline3\nline4\nline5\n"
      (":2,3copy0")
      "line2\n[l]ine3\nline1\nline2\nline3\nline4\nline5\n"))
  (ert-info ("Copy to intermediate line")
    (evil-test-buffer
      "[l]ine1\nline2\nline3\nline4\nline5\n"
      (":2,4copy2")
      "line1\nline2\nline2\nline3\n[l]ine4\nline3\nline4\nline5\n"))
  (ert-info ("Copy to current line")
    (evil-test-buffer
      "line1\nline2\nline3\nli[n]e4\nline5\n"
      (":2,4copy.")
      "line1\nline2\nline3\nline4\nline2\nline3\n[l]ine4\nline5\n")))

(ert-deftest evil-test-move ()
  :tags '(evil ex)
  "Test `evil-move'."
  (ert-info ("Move to last line")
    (evil-test-buffer
      "[l]ine1\nline2\nline3\nline4\nline5\n"
      (":2,3move$")
      "line1\nline4\nline5\nline2\n[l]ine3\n"))
  (ert-info ("Move to last incomplete line")
    (evil-test-buffer
      "[l]ine1\nline2\nline3\nline4\nline5"
      (":2,3move$")
      "line1\nline4\nline5\nline2\n[l]ine3\n"))
  (ert-info ("Move incomplete line to last incomplete line")
    (evil-test-buffer
      "[l]ine1\nline2\nline3\nline4\nline5"
      (":4,5move$")
      "line1\nline2\nline3\nline4\n[l]ine5\n"))
  (ert-info ("Move to first line")
    (evil-test-buffer
      "[l]ine1\nline2\nline3\nline4\nline5\n"
      (":2,3move0")
      "line2\n[l]ine3\nline1\nline4\nline5\n"))
  (ert-info ("Move to intermediate line")
    (evil-test-buffer
      "[l]ine1\nline2\nline3\nline4\nline5\n"
      (":2,4move2")
      "line1\nline2\nline3\n[l]ine4\nline5\n"))
  (ert-info ("Move to other line")
    (evil-test-buffer
      "[l]ine1\nline2\nline3\nline4\nline5\n"
      (":2,3move4")
      "line1\nline4\nline2\n[l]ine3\nline5\n"))
  (ert-info ("Move to current line")
    (evil-test-buffer
      "line1\nline2\nline3\nli[n]e4\nline5\n"
      (":2,4move.")
      "line1\nline2\nline3\n[l]ine4\nline5\n")))

(ert-deftest evil-test-write ()
  :tags '(evil ex)
  "Test `evil-write'."
  (ert-info ("Write open file")
    (evil-with-temp-file filename "line1\nline2\nline3\n"
      (evil-test-buffer
        ((vconcat ":e " filename [return]))
        "[l]ine1\nline2\nline3\n"
        ("Galine4\nline5\n" [escape])
        "line1\nline2\nline3\nline4\nline5\n"
        (":w")
        (file filename "line1\nline2\nline3\nline4\nline5\n"))))
  (ert-info ("Write current buffer to new file")
    (let ((filename (make-temp-file "evil-test-write")))
      (evil-test-buffer
        "[l]ine1\nline2\nline3\nline4\nline5\n"
        (delete-file filename)
        ((vconcat ":w " filename [return]))
        (file filename "line1\nline2\nline3\nline4\nline5\n")
        (delete-file filename))))
  (ert-info ("Write part of a buffer")
    (let ((filename (make-temp-file "evil-test-write")))
      (evil-test-buffer
        "[l]ine1\nline2\nline3\nline4\nline5\n"
        (delete-file filename)
        ((vconcat ":2,3w " filename [return]))
        (file filename "line2\nline3\n")
        (delete-file filename))))
  (ert-info ("Appending a file")
    (let ((filename (make-temp-file "evil-test-write")))
      (evil-test-buffer
        "[l]ine1\nline2\nline3\nline4\nline5\n"
        (delete-file filename)
        ((vconcat ":4w " filename [return]))
        (file filename "line4\n")
        ((vconcat ":1,2w >>" filename [return]))
        (file filename "line4\nline1\nline2\n")
        ((vconcat ":w >> " filename [return]))
        (file filename
              "line4\nline1\nline2\nline1\nline2\nline3\nline4\nline5\n")
        (delete-file filename)))))

(ert-deftest evil-test-ex-sort ()
  :tags '(evil ex)
  "Text ex command :sort `evil-ex-sort`."
  (ert-info ("Plain sort")
    (evil-test-buffer
      "[z]zyy\ntest\ntEst\ntesT\nTEST\ntest\n"
      (":sort")
      "[T]EST\ntEst\ntesT\ntest\ntest\nzzyy\n"))
  (ert-info ("Reverse sort")
    (evil-test-buffer
      "[z]zyy\ntest\ntEst\ntesT\nTEST\ntest\n"
      (":sort!")
      "[z]zyy\ntest\ntest\ntesT\ntEst\nTEST\n"))
  (ert-info ("case insensitive")
    (evil-test-buffer
      "[z]zyy\ntest\ntEst\ntesT\nTEST\ntest\n"
      (":sort i")
      "[t]est\ntEst\ntesT\nTEST\ntest\nzzyy\n"))
  (ert-info ("unique")
    (evil-test-buffer
      "[z]zyy\ntest\ntEst\ntesT\nTEST\ntest\n"
      (":sort u")
      "[T]EST\ntEst\ntesT\ntest\nzzyy\n"))
  (ert-info ("case insensitive and unique")
    (evil-test-buffer
      "[z]zyy\ntest\ntEst\ntesT\nTEST\ntest\n"
      (":sort iu")
      "[t]est\nzzyy\n")))

;;; Command line window

(ert-deftest evil-test-command-window-ex ()
  "Test command line window for ex commands"
  (let (evil-ex-history)
    (evil-test-buffer
      "[f]oo foo foo"
      (":s/foo/bar" [return])
      "[b]ar foo foo"
      (":s/foo/baz" [return])
      "[b]ar baz foo"
      ("q:")
      "s/foo/bar\ns/foo/baz\n[]\n"
      ("kk:s/bar/quz" [return])
      "[s]/foo/quz\ns/foo/baz\n"
      ("fzrx")
      "s/foo/qu[x]\ns/foo/baz\n"
      ([return])
      "[b]ar baz qux"
      (should (equal (car evil-ex-history)
                     "s/foo/qux")))))

(ert-deftest evil-test-command-window-recursive ()
  "Test that recursive command windows shouldn't be allowed"
  (let ((evil-command-window-height 0))
    (evil-test-buffer
      "[f]oo foo foo"
      (":s/foo/bar" [return])
      ("q:")
      (should-error (execute-kbd-macro "q:")))))

(ert-deftest evil-test-command-window-noop ()
  "Test that executing a blank command does nothing"
  (evil-test-buffer
    "[f]oo foo foo"
    ("q:")
    "[]\n"
    ([return])
    "[f]oo foo foo"))

(ert-deftest evil-test-command-window-multiple ()
  "Test that multiple command line windows can't be visible at the same time"
  (let ((evil-command-window-height 0))
    (evil-test-buffer
      "[f]oo foo foo"
      ("q:")
      (let ((num-windows (length (window-list))))
        (select-window (previous-window))
        (execute-kbd-macro "q:")
        (should (= (length (window-list)) num-windows))))))

(defmacro evil-with-both-search-modules (&rest body)
  `(mapc (lambda (search-module)
           (setq evil-search-forward-history nil
                 evil-search-backward-history nil
                 evil-ex-search-history nil)
           (evil-select-search-module 'evil-search-module search-module)
           ,@body)
         '(isearch evil-search)))

(ert-deftest evil-test-command-window-search-history ()
  "Test command window with forward and backward search history"
  (let ((evil-search-module 'isearch))
    (evil-test-buffer
      "[f]oo bar baz qux one two three four"
      ("/qux" [return])
      "foo bar baz [q]ux one two three four"
      ("/three" [return])
      "foo bar baz qux one two [t]hree four"
      ("?bar" [return])
      "foo [b]ar baz qux one two three four"
      ("/four" [return])
      "foo bar baz qux one two three [f]our"
      ("?baz" [return])
      "foo bar [b]az qux one two three four"
      ("q/")
      "qux\nthree\nfour\n[]\n"
      ("k" [return])
      "foo bar baz qux one two three [f]our"
      ("0N")
      "foo bar baz qux one two three [f]our"
      ("q?")
      "bar\nbaz\n[]\n"
      ("k$rr" [return])
      "foo [b]ar baz qux one two three four"
      (should-error
       (progn (execute-kbd-macro "q/iNOT THERE")
              (execute-kbd-macro [return])))
      "foo [b]ar baz qux one two three four")))

(ert-deftest evil-test-command-window-search-word ()
  "Test command window history when searching for word under cursor"
  (let ((evil-search-module 'isearch))
    (evil-test-buffer
      "[f]oo bar foo bar foo"
      ("**")
      "foo bar foo bar [f]oo"
      ("B#")
      "foo [b]ar foo bar foo"
      ("q/k" [return])
      "foo bar [f]oo bar foo"
      ("q?k" [return])
      "foo [b]ar foo bar foo")))

;;; Utilities

(ert-deftest evil-test-parser ()
  "Test `evil-parser'"
  (let ((grammar '((number "[0-9]+" #'string-to-number)
                   (plus "\\+" #'intern)
                   (minus "-" #'intern)
                   (operator
                    plus
                    minus)
                   (sign
                    ((\? operator) #'$1))
                   (signed-number
                    (sign number))
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
                       . " a 3")))
      (should (equal (evil-parser "1" 'signed-number grammar t t)
                     '((signed-number (sign "") (number "1")) . ""))))
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

(ert-deftest evil-test-delimited-arguments ()
  "Test `evil-delimited-arguments'"
  :tags '(evil util)
  (ert-info ("Any number of arguments")
    (should (equal (evil-delimited-arguments "/a/b/c/")
                   '("a" "b" "c")))
    (should (equal (evil-delimited-arguments "/a/b/c")
                   '("a" "b" "c")))
    (should (equal (evil-delimited-arguments "/a/b//")
                   '("a" "b" "")))
    (should (equal (evil-delimited-arguments "/a///")
                   '("a" "" "")))
    (should (equal (evil-delimited-arguments "/a/   ")
                   '("a" "   ")))
    (should (equal (evil-delimited-arguments "/a/")
                   '("a")))
    (should (equal (evil-delimited-arguments "//b//")
                   '("" "b" "")))
    (should (equal (evil-delimited-arguments "/a//c")
                   '("a" "" "c")))
    (should (equal (evil-delimited-arguments "////")
                   '("" "" "")))
    (should (equal (evil-delimited-arguments "/")
                   nil))
    (should (equal (evil-delimited-arguments "    ")
                   nil))
    (should (equal (evil-delimited-arguments "")
                   nil)))
  (ert-info ("Two arguments")
    (should (equal (evil-delimited-arguments "/a/b/c" 2)
                   '("a" "b/c")))
    (should (equal (evil-delimited-arguments "/a/b/" 2)
                   '("a" "b")))
    (should (equal (evil-delimited-arguments "/a/b" 2)
                   '("a" "b")))
    (should (equal (evil-delimited-arguments "/a//" 2)
                   '("a" "")))
    (should (equal (evil-delimited-arguments "/a/   " 2)
                   '("a" "   ")))
    (should (equal (evil-delimited-arguments "/a/" 2)
                   '("a" nil)))
    (should (equal (evil-delimited-arguments "/a" 2)
                   '("a" nil)))
    (should (equal (evil-delimited-arguments "    " 2)
                   '(nil nil)))
    (should (equal (evil-delimited-arguments "" 2)
                   '(nil nil))))
  (ert-info ("One argument")
    (should (equal (evil-delimited-arguments "/a/b/c" 1)
                   '("a/b/c")))
    (should (equal (evil-delimited-arguments "/a/   " 1)
                   '("a")))
    (should (equal (evil-delimited-arguments "/a/" 1)
                   '("a")))
    (should (equal (evil-delimited-arguments "/a" 1)
                   '("a")))
    (should (equal (evil-delimited-arguments "/" 1)
                   '(nil)))
    (should (equal (evil-delimited-arguments "    " 1)
                   '(nil)))
    (should (equal (evil-delimited-arguments "" 1)
                   '(nil))))
  (ert-info ("Zero arguments")
    (should (equal (evil-delimited-arguments "/a" 0)
                   nil))
    (should (equal (evil-delimited-arguments "/" 0)
                   nil))
    (should (equal (evil-delimited-arguments "    " 0)
                   nil))
    (should (equal (evil-delimited-arguments "" 0)
                   nil))))

(ert-deftest evil-test-concat-charsets ()
  "Test `evil-concat-charsets'"
  :tags '(evil util)
  (ert-info ("Bracket")
    (should (equal (evil-concat-charsets "abc" "]def")
                   "]abcdef")))
  (ert-info ("Complement")
    (should (equal (evil-concat-charsets "^abc" "def")
                   "^abcdef"))
    (should (equal (evil-concat-charsets "^abc" "^def")
                   "^abcdef")))
  (ert-info ("Hyphen")
    (should (equal (evil-concat-charsets "abc" "-def")
                   "-abcdef"))
    (should (equal (evil-concat-charsets "^abc" "-def")
                   "^-abcdef")))
  (ert-info ("Newline")
    (should (equal (evil-concat-charsets "^ \t\r\n" "[:word:]_")
                   "^ \t\r\n[:word:]_"))))

(ert-deftest evil-test-properties ()
  "Test `evil-get-property' and `evil-put-property'"
  :tags '(evil util)
  (let (alist)
    (ert-info ("Set properties")
      (evil-put-property 'alist 'wibble :foo t)
      (should (equal alist '((wibble . (:foo t)))))
      (evil-put-property 'alist 'wibble :bar nil)
      (should (equal alist '((wibble . (:foo t :bar nil)))))
      (evil-put-property 'alist 'wobble :foo nil :bar nil :baz t)
      (should (equal alist '((wobble . (:foo nil :bar nil :baz t))
                             (wibble . (:foo t :bar nil))))))
    (ert-info ("Get properties")
      (should (evil-get-property alist 'wibble :foo))
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

(ert-deftest evil-transform-vim-style-regexp ()
  "Test `evil-transform-vim-style-regexp'"
  (dolist (repl '((?s . "[[:space:]]")
                  (?S . "[^[:space:]]")
                  (?d . "[[:digit:]]")
                  (?D . "[^[:digit:]]")
                  (?x . "[[:xdigit:]]")
                  (?X . "[^[:xdigit:]]")
                  (?o . "[0-7]")
                  (?O . "[^0-7]")
                  (?a . "[[:alpha:]]")
                  (?A . "[^[:alpha:]]")
                  (?l . "[a-z]")
                  (?L . "[^a-z]")
                  (?u . "[A-Z]")
                  (?U . "[^A-Z]")
                  (?y . "\\s")
                  (?Y . "\\S")
                  (?w . "\\w")
                  (?W . "\\W")))
    (ert-info ((format "Test transform from '\\%c' to '%s'"
                       (car repl) (cdr repl)))
      (should (equal (evil-transform-vim-style-regexp
                      (concat "xxx\\"
                              (char-to-string (car repl))
                              "\\"
                              (char-to-string (car repl))
                              "\\\\"
                              (char-to-string (car repl))
                              "\\\\\\"
                              (char-to-string (car repl))
                              "yyy"))
                     (concat "xxx"
                             (cdr repl)
                             (cdr repl)
                             "\\\\"
                             (char-to-string (car repl))
                             "\\\\"
                             (cdr repl)
                             "yyy"))))))

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

(ert-deftest evil-test-black-hole-register ()
  :tags '(evil)
  (ert-info ("Test \"_ on delete word")
    (evil-test-buffer
      "[E]vil evil is awesome."
      ("dw\"_dwP")
      "Evil[ ]is awesome."))
  (ert-info ("Test \"_ on delete line")
    (evil-test-buffer
      "[T]his line is a keeper!\nThis line is not."
      ("dd\"_ddP")
      "[T]his line is a keeper!"))
  (ert-info ("Test \"_ on delete region")
    (evil-test-buffer
      "<This region is a keeper>!\nThis line is not."
      ("d\gg\"_dGP")
      "This region is a keepe[r]")))

(ert-deftest evil-test-pasteable-macros ()
  "Test if we can yank and paste macros containing
                  <escape>"
  :tags '(evil)
  (ert-info ("Execute yanked macro")
    (evil-test-buffer
      "[i]foo\e"
      ("\"qd$@q\"qp"
       "fooifoo\e")))
  (ert-info ("Paste recorded marco")
    (evil-test-buffer
      ""
      (evil-set-register ?q (vconcat "ifoo" [escape]))
      ("@q\"qp")
      "fooifoo\e")))

(ert-deftest evil-test-forward-symbol ()
  :tags '(evil)
  (ert-info ("Test symbol deletion")
    (evil-test-buffer
      "(test [t]his (hello there) with dao)"
      ("dao")
      "(test [(]hello there) with dao)"))
  (ert-info ("Test symbol motion")
    (evil-test-buffer
      "(test[ ](hello there) with dao)"
      (should (eq 0 (forward-evil-symbol 1)))
      "(test ([h]ello there) with dao)"
      (should (eq 0 (forward-evil-symbol 1)))
      "(test (hello[ ]there) with dao)"))
  (ert-info ("Test dio on whitespace")
    (evil-test-buffer
      "(test[ ]dio with whitespace)"
      ("dio")
      "(test[d]io with whitespace)"))
  (ert-info ("Test dao/dio with empty lines")
    (evil-test-buffer
      "there are two lines in this file\n[\n]and some whitespace between them"
      ("dao")
      "there are two lines in this file\n[a]nd some whitespace between them")
    (evil-test-buffer
      "here are another two lines\n[\n]with a blank line between them"
      ("dio")
      "here are another two lines\n[w]ith a blank line between them"))
  (ert-info ("Test dao/dio with empty lines and punctuation")
    (evil-test-buffer
      "These two lines \n[\n]!have punctuation on them"
      ("dao")
      "These two lines \n[!]have punctuation on them")))

(ert-deftest evil-test-jump ()
  :tags '(evil jumps)
  (let ((evil--jumps-buffer-targets "\\*\\(new\\|scratch\\|test\\)\\*"))
    (ert-info ("Test jumping backward and forward in a single buffer")
      (evil-test-buffer
        "[z] z z z z z z z z z"
        ("/z" [return])
        "z [z] z z z z z z z z"
        ("nnnn")
        "z z z z z [z] z z z z"
        ("\C-o")
        "z z z z [z] z z z z z"
        ("\C-o")
        "z z z [z] z z z z z z"
        ("\C-i\C-i")
        "z z z z z [z] z z z z"))
    (ert-info ("Test jumping backward and forward across buffers")
      (evil-test-buffer
        "[z] z z z z z z z z z"
        (":new" [return] "inew buffer" [escape])
        "new buffe[r]"
        ("\C-o")
        "[z] z z z z z z z z z"
        ("\C-i")
        "new buffe[r]"))
    (ert-info ("Test jumping backward and forward with counts")
      (evil-test-buffer
        "[z] z z z z z z z z z"
        ("/z" [return] "nnnn")
        "z z z z z [z] z z z z"
        ("3\C-o")
        "z z [z] z z z z z z z"
        ("2\C-i")
        "z z z z [z] z z z z z"
        ))
    (ert-info ("Jump list branches off when new jump is set")
      (evil-test-buffer
        "[z] z z z z z z z"
        ("/z" [return] "nnnn4\C-o") ;; adds a bunch of jumps after the 2nd z
        "z [z] z z z z z z"
        ("/z" [return]) ;; sets a new jump, list should be reset
        "z z [z] z z z z z"
        ("\C-o")
        "z [z] z z z z z z"
        ("3\C-i") ;; even after jumping forward 3 times it can't get past the 3rd z
        "z z [z] z z z z z"))))

(ert-deftest evil-test-abbrev-expand ()
  :tags '(evil abbrev)
  (ert-info ("Test abbrev expansion on insert state exit")
    (define-abbrev-table 'global-abbrev-table
      '(("undef" "undefined"))) ;; add global abbrev
    (evil-test-buffer
     "foo unde[f] bar"
     ("a" [escape])
     "foo undefine[d] bar") ;; 'undef' should be expanded
    (evil-test-buffer
     "fo[o] undef bar"
     ("a" [escape])
     "fo[o] undef bar") ;; 'foo' shouldn't be expanded, it's not an abbrev
    (kill-all-abbrevs) ;; remove abbrevs
    (evil-test-buffer
     "foo unde[f] bar"
     ("a" [escape])
     "foo unde[f] bar") ;; 'undef' is not an abbrev, shouldn't be expanded
    (setq abbrevs-changed nil)))

(ert-deftest evil-test-text-object-macro ()
  :tags '(evil abbrev)
  (ert-info ("Test pipe character and other delimiters as object delimiters")
    ;; This is the macro that broke after pull #747.
    (defmacro evil-test-define-and-bind-text-object (name key start-regex end-regex)
      (let ((inner-name (make-symbol (concat "evil-inner-" name)))
            (outer-name (make-symbol (concat "evil-a-" name))))
        `(progn
           (evil-define-text-object ,inner-name (count &optional beg end type)
             (evil-select-paren ,start-regex ,end-regex beg end type count nil))
           (evil-define-text-object ,outer-name (count &optional beg end type)
             (evil-select-paren ,start-regex ,end-regex beg end type count t))
           (define-key evil-inner-text-objects-map ,key #',inner-name)
           (define-key evil-outer-text-objects-map ,key #',outer-name))))
    (evil-test-define-and-bind-text-object "pipe" "|" "|" "|")
    (evil-test-define-and-bind-text-object "rackety" "#" "#|" "|#")

    (evil-test-buffer
     "#|this i[s] a test #|with rackety|# multiline
  and nestable comments|#"
     ("vi#")
     "#|<this is a test #|with rackety|# multiline
  and nestable comments>|#")
    (evil-test-buffer
     "| foo | aoe[u] | bar |"
     ("vi|")
     "| foo |< aoeu >| bar |"
     ("a|")
     "| foo <| aoeu |> bar |"
     ("a|")
     "<| foo | aoeu | bar |>")
    (evil-test-buffer
     "| foo | aoe[u] | bar |"
     ("ci|testing" [escape])
     "| foo |testing| bar |")))

(ert-deftest evil-test-undo-kbd-macro ()
  "Test if evil can undo the changes made by a keyboard macro 
when an error stops the execution of the macro"
  :tags '(evil undo kbd-macro)
  (ert-info ("When kbd-macro goes to the end of buffer")
    (evil-test-buffer
 	 "[l]ine 1\nline 2\nline 3\nline 4"
     (evil-set-register ?q "jdd")
     ("jdd")
     (should-error (execute-kbd-macro "2@q"))
 	 ("uu")
 	 "line 1\n[l]ine 2\nline 3\nline 4"))
  (ert-info ("When kbd-macro goes to the end of line")
    (evil-test-buffer
 	 "[f]ofof"
     (evil-set-register ?q "lx")
     ("lx")
     (should-error (execute-kbd-macro "2@q"))
 	 ("uu")
 	 "f[o]fof"))
  (ert-info ("When kbd-macro goes to the beginning of buffer")
    (evil-test-buffer
 	 "line 1\nline 2\n[l]ine 3"
     (evil-set-register ?q "kx")
     ("kx")
     (should-error (execute-kbd-macro "2@q"))
 	 ("uu")
 	 "line 1\n[l]ine 2\nline 3")))

(ert-deftest evil-test-visual-update-x-selection ()
  "Test `evil-visual-update-x-selection'."
  :tags '(evil)
  (ert-info ("Buffer argument isn't a live buffer")
    ; create buffer in normal mode, so we don't try to actually copy anything to
    ; the X selection.
    (let ((buf (evil-test-buffer-from-string "foobar")))
      (kill-buffer buf)
      ; should not raise an "Selecting deleted buffer" error
      (evil-visual-update-x-selection buf))))

;;; Core

(ert-deftest evil-test-initial-state ()
  "Test `evil-initial-state'"
  :tags '(evil core)
  (define-derived-mode test-1-mode prog-mode "Test1")
  (define-derived-mode test-2-mode test-1-mode "Test2")
  (evil-set-initial-state 'test-1-mode 'insert)
  (ert-info ("Check default state")
    (should (eq (evil-initial-state 'prog-mode 'normal) 'normal)))
  (ert-info ("Basic functionality 1")
    (should (eq (evil-initial-state 'test-1-mode) 'insert)))
  (ert-info ("Basic functionality 2")
    (evil-test-buffer
      "abc\ndef\n"
      (test-1-mode)
      (should (eq evil-state 'insert))))
  (ert-info ("Inherit initial state from a parent")
    (evil-test-buffer
      "abc\ndef\n"
      (test-2-mode)
      (should (eq evil-state 'insert))))
  (evil-set-initial-state 'test-1-mode nil)
  (ert-info ("Check for inheritance loops")
    (evil-test-buffer
      "abc\ndef\n"
      (unwind-protect
          (let ((major-mode 'test-2-mode))
            (put 'test-1-mode 'derived-mode-parent 'test-2-mode)
            ;; avoid triggering all of the hooks here, some of which might get
            ;; caught in loops depending on the environment. settings major-mode
            ;; is sufficient for `evil-initial-state-for-buffer' to work.
            (should-error (evil-initial-state-for-buffer)))
        (put 'test-1-mode 'derived-mode-parent 'prog-mode))))
  (defalias 'test-1-alias-mode 'test-1-mode)
  (define-derived-mode test-3-mode test-1-alias-mode "Test3")
  (evil-set-initial-state 'test-1-mode 'insert)
  (ert-info ("Check inheritance from major mode aliases")
    "abc\ndef\n"
    (test-3-mode)
    (should (eq evil-state 'insert))))

(provide 'evil-tests)

;;; evil-tests.el ends here
