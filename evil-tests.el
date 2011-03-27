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
and `evil-local-mode' is enabled.

An alternative buffer string can be specified with the
:text keyword before the body code."
  (declare (indent defun)
           (debug t))
  (let ((text ";; This buffer is for notes you don't want to save, \
and for Lisp evaluation.\n;; If you want to create a file, visit \
that file with C-x C-f,\n;; then enter the text in that file's own \
buffer.\n\nBelow the empty line.") keyword)
    (while (keywordp (car-safe body))
      (setq keyword (pop body))
      (cond
       ((eq keyword :text)
        (setq text (pop body)))
       (t
        (pop body))))
    `(let ((kill-ring kill-ring)
           (kill-ring-yank-pointer kill-ring-yank-pointer)
           x-select-enable-clipboard
           message-log-max)
       (save-window-excursion
         (with-temp-buffer
           (switch-to-buffer-other-window (current-buffer))
           (buffer-enable-undo)
           (evil-local-mode 1)
           (delete-region (point-min) (point-max))
           (insert ,text)
           (goto-char (point-min))
           ,@body)))))

(defmacro evil-test-code-buffer (&rest body)
  "Execute BODY in a temporary buffer.
The buffer contains a C \"Hello world\" program,
and `evil-local-mode' is enabled."
  (declare (indent defun)
           (debug t))
  `(evil-test-buffer
     :text "#include <stdio.h>\n#include <stdlib.h>\n\n\
int main(int argc, char** argv)     \n{\n\
  printf(\"Hello world\\n\");\n\
  return EXIT_SUCCESS;\n     \n}\n"
     ,@body))

(defmacro evil-test-paragraph-buffer (&rest body)
  "Execute BODY in a temporary buffer.
The buffer contains the familiar *scratch* message,
and `evil-local-mode' is enabled."
  (declare (indent defun)
           (debug t))
  (let ((beg-newl 0)
        (end-newl 0)
        arg key keys)
    (while (keywordp (car-safe body))
      (setq key (pop body)
            arg (pop body))
      (cond
       ((eq key :begin-newlines)
        (setq beg-newl arg))
       ((eq key :end-newlines)
        (setq end-newl arg))
       (t
        (setq keys (append keys (list key arg))))))
    `(evil-test-buffer
       ,@keys
       :text ,(format
               "%s;; This buffer is for notes you don't want to save, \
and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.\n\n\nSingle Line\n\n\n
;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.%s"
               (make-string beg-newl ?\n)
               (make-string end-newl ?\n))
       ,@body)))

(defun evil-test-text
  (before after &optional before-predicate after-predicate)
  "Verifies the text around point.
BEFORE is the expected text before point, and AFTER is
the text after point. BEFORE-PREDICATE is a predicate function
to execute at the beginning of the text, and AFTER-PREDICATE
is executed at the end."
  (when before
    (if (functionp before)
        (setq before-predicate before
              before nil)
      (should (string= (buffer-substring
                        (- (point) (length before))
                        (point))
                       before))))
  (when after
    (if (functionp after)
        (setq after-predicate after
              after nil)
      (should (string= (buffer-substring
                        (point)
                        (+ (point) (length after)))
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

(defun evil-test-text-lines (&rest line-tests)
  "Calls `evil-test-text' once for each element of `line-tests' on successive lines.
The first element of `line-tests' is the test for the
current-line. The other elements are tested on the successive
line while (point) as always moved to the same column as in the
first line via `move-to-column'."
  (let ((col (current-column)))
    (save-excursion
      (dolist (test line-tests)
        (move-to-column col)
        (ert-info ((format "Line: %s column: %s"
                           (line-number-at-pos)
                           (current-column)))
          (apply #'evil-test-text test))
        (forward-line)))))

(defmacro evil-test-region
  (string &optional end-string before-predicate after-predicate)
  "Verify that the region corresponds to STRING."
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
  "Verify that OVERLAY corresponds to STRING."
  (declare (indent defun))
  `(progn
     (save-excursion
       (goto-char (overlay-start ,overlay))
       (evil-test-text nil (or ,string ,end-string) ,before-predicate))
     (save-excursion
       (goto-char (overlay-end ,overlay))
       (evil-test-text (or ,end-string ,string) nil nil ,after-predicate))))

(defmacro evil-test-macro
  (keys &optional before after before-predicate after-predicate)
  "Execute keybard macro KEYS and verify the text around point.
KEYS can be a string, a vector, a form, or a list of these.
See `evil-test-text' for an explanation of the other arguments."
  (declare (indent defun))
  `(let ((keys ',keys))
     (when (listp keys)
       (cond
        ((and (symbolp (car-safe keys))
              (fboundp (car-safe keys)))
         (eval keys)
         (setq keys nil))
        (t
         (setq keys (eval `(vconcat ,@keys))))))
     (when keys
       (execute-kbd-macro keys))
     (evil-test-text ,before ,after ,before-predicate ,after-predicate)))

(defmacro evil-test-buffer-edit
  (keys &optional before after before-predicate after-predicate)
  "The same as `evil-test-macro', but starts with a new
unchanged test-buffer in Normal state."
  (declare (indent defun))
  `(evil-test-buffer
     (evil-test-change-state 'normal)
     (evil-test-macro ,keys
       ,before ,after ,before-predicate ,after-predicate)))

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
            (list evil-operator-shortcut-map
                  evil-operator-state-local-map
                  evil-operator-state-map
                  evil-normal-state-local-map
                  evil-normal-state-map
                  evil-motion-state-local-map
                  evil-motion-state-map))))
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
    (evil-test-change-state 'emacs)
    (evil-test-change-state 'replace)
    (evil-test-change-state 'normal)))

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
    ;; TODO: this must be done better
    (ert-info ("Disable the state's own keymaps so that the
suppression keymap comes first")
      (setq evil-motion-state nil
            evil-motion-state-local nil
            evil-operator-state nil
            evil-operator-state-local nil))
    (should (eq (key-binding "y") 'undefined))
    (should (eq (key-binding "u") 'undefined))
    (should (eq (key-binding "e") 'undefined))
    (ert-info ("Don't insert text")
      ;; may or may not signal an error, depending on batch mode
      (condition-case nil
          (execute-kbd-macro "yue")
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
                         (point)))
           (overlay (make-overlay 1 1)))
      (ert-info ("Return the beginning and end unchanged
if they are the same")
        (should (equal (evil-normalize 1 1 'exclusive)
                       (list 1 1 'exclusive))))
      (ert-info ("expand to `inclusive' if the end position
is at the beginning of a line")
        (should (equal (evil-normalize (1+ first-line) second-line 'exclusive)
                       (list (1+ first-line) (1- second-line) 'inclusive
                             :expanded t))))
      (ert-info ("expand to `line' if both the beginning and end
are at the beginning of a line")
        (should (equal (evil-normalize first-line second-line 'exclusive)
                       (list first-line second-line 'line
                             :expanded t))))
      (ert-info ("Measure as the strict difference between the end
and the beginning")
        (should (string= (evil-describe 1 1 'exclusive)
                         "0 characters"))
        (should (string= (evil-describe 1 2 'exclusive)
                         "1 character"))
        (should (string= (evil-describe 5 2 'exclusive)
                         "3 characters")))
      (ert-info ("Expand and measure overlay")
        (evil-set-type overlay 'exclusive)
        (should (string= (evil-describe-overlay overlay)
                         "0 characters"))
        (move-overlay overlay 1 3)
        (evil-expand-overlay overlay)
        (should (string= (evil-describe-overlay overlay)
                         "2 characters"))
        (evil-contract-overlay overlay)
        (should (string= (evil-describe-overlay overlay)
                         "2 characters"))
        (ert-info ("Normalize overlay")
          (move-overlay overlay (1+ first-line) second-line)
          (evil-normalize-overlay overlay)
          (should (= (overlay-start overlay) (1+ first-line)))
          (should (= (overlay-end overlay) (1- second-line)))
          (should (eq (evil-type overlay) 'inclusive))
          (should (overlay-get overlay :expanded)))
        (ert-info ("Contract overlay")
          (evil-contract-overlay overlay)
          (should-not (overlay-get overlay :expanded)))))))

(ert-deftest evil-test-inclusive-type ()
  "Expand and contract the `inclusive' type"
  :tags '(evil)
  (evil-test-buffer
    (let ((overlay (make-overlay 1 1)))
      (ert-info ("Include the ending character")
        (should (equal (evil-expand 1 1 'inclusive)
                       '(1 2 inclusive :expanded t))))
      (ert-info ("Don't mind if positions are in wrong order")
        (should (equal (evil-expand 5 2 'inclusive)
                       '(2 6 inclusive :expanded t))))
      (ert-info ("Exclude the ending character when contracting")
        (should (equal (evil-contract 1 2 'inclusive)
                       '(1 1 inclusive :expanded nil))))
      (ert-info ("Don't mind positions order when contracting")
        (should (equal (evil-contract 6 2 'inclusive)
                       '(2 5 inclusive :expanded nil))))
      (ert-info ("Measure as one more than the difference")
        (should (string= (evil-describe 1 1 'inclusive)
                         "1 character"))
        (should (string= (evil-describe 5 2 'inclusive)
                         "4 characters")))
      (ert-info ("Expand overlay")
        (evil-set-type overlay 'inclusive)
        (evil-expand-overlay overlay)
        (should (= (overlay-start overlay) 1))
        (should (= (overlay-end overlay) 2))
        (should (overlay-get overlay :expanded)))
      (ert-info ("Contract overlay")
        (move-overlay overlay 1 4)
        (evil-contract-overlay overlay)
        (should (= (overlay-start overlay) 1))
        (should (= (overlay-end overlay) 3))
        (should-not (overlay-get overlay :expanded))))))

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
                         (point)))
           (overlay (make-overlay 1 1)))
      (ert-info ("Expand to the whole first line")
        (should (equal (evil-expand first-line first-line 'line)
                       (list first-line second-line 'line :expanded t)))
        (should (string= (evil-describe first-line first-line 'line)
                         "1 line")))
      (ert-info ("Expand to the two first lines")
        (should (equal (evil-expand first-line second-line 'line)
                       (list first-line third-line 'line :expanded t)))
        (should (string= (evil-describe first-line second-line 'line)
                         "2 lines")))
      (ert-info ("Expand overlay")
        (evil-set-type overlay 'line)
        (evil-expand-overlay overlay)
        (should (= (overlay-start overlay) first-line))
        (should (= (overlay-end overlay) second-line))
        (should (overlay-get overlay :expanded)))
      (ert-info ("Restore overlay")
        (evil-contract-overlay overlay)
        (should (= (overlay-start overlay) 1))
        (should (= (overlay-end overlay) 1))
        (should-not (overlay-get overlay :expanded))))))

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
                       (list first-line (1+ (1+ third-line)) 'block :expanded t)))
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
                       (list first-line (1+ third-line) 'block :expanded nil)))))))

(ert-deftest evil-test-type-transform ()
  "Test `evil-transform'"
  :tags '(evil)
  (evil-test-buffer
    (ert-info ("Return positions unchanged when passed nil for
TYPE or TRANSFORM")
      (should (equal (evil-transform 1 2 'block nil)
                     '(1 2 block)))
      (should (equal (evil-transform 1 2 nil 'expand)
                     '(1 2)))
      (should (equal (evil-transform 1 2 nil nil)
                     '(1 2))))
    (ert-info ("Accept markers, but return positions")
      (should (equal (evil-transform (move-marker (make-marker) 1) 1
                                     'inclusive 'expand)
                     '(1 2 inclusive :expanded t)))
      (should (equal (evil-transform (move-marker (make-marker) 1) 2
                                     nil nil)
                     '(1 2))))))

(ert-deftest evil-test-type-modifiers ()
  "Test type modifiers like \"dv}\""
  :tags '(evil)
  (let ((text "Above some line\n\nBelow some empty line"))
    (ert-info ("Change `inclusive' motions to `exclusive'")
      (evil-test-buffer
        :text text
        (evil-test-change-state 'normal)
        (evil-test-macro "dve"
          'bobp "e some line")))
    (ert-info ("Change `exclusive' motions to `inclusive'")
      (evil-test-buffer
        :text text
        (evil-test-change-state 'normal)
        (evil-test-macro "wdv}"
          "Above" " \nBelow some empty line")))
    (ert-info ("Change type to `line'")
      (evil-test-buffer
        :text text
        (evil-test-change-state 'normal)
        (evil-test-macro "wdV}"
          'bobp "Below some empty line")))))

;;; Repeat system

(ert-deftest evil-test-normalize-repeat-info ()
  "Verify normalize-repeat-info"
  :tags '(evil)
  (ert-info ("Single array")
    (should (equal (evil-normalize-repeat-info
                    '("abc"))
                   '([?a ?b ?c]))))
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
  "Executes a sequence of keys and verifies that `evil-repeat-info'
records them correctly. KEYS is the sequence of keys to execute.
RECORDED is the expected sequence of recorded events. If nil,
KEYS is used."
  (execute-kbd-macro keys)
  (should (equal (evil-normalize-repeat-info evil-repeat-info)
                 (list (vconcat (or recorded keys))))))

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

(ert-deftest evil-test-repeat ()
  "Repeat several editing commands."
  :tags '(evil)
  (ert-info ("Repeat replace")
    (evil-test-buffer-edit ("rX" [right right] ".")
      "X;" "XThis" 'bobp))

  (ert-info ("Repeat replace with count")
    (evil-test-buffer-edit ("2rX" [right right] ".")
      "XX X" "Xis " 'bobp))

  (ert-info ("Repeat replace without count with a new count")
    (evil-test-buffer-edit ("rX" [right right] "13.")
      "X;XXXXXXXXXXXX" "Xis for" 'bobp))

  (ert-info ("Repeat replace with count replacing original count")
    (evil-test-buffer-edit ("10rX" [right right] "20.")
      "XXXXXXXXXXfXXXXXXXXXXXXXXXXXXX" "X don't " 'bobp)))

(ert-deftest evil-test-cmd-replace-char ()
  "Calling `evil-replace-char' should replace characters."
  :tags '(evil)
  (evil-test-buffer-edit "r5"
    'bobp "5; This")
  (evil-test-buffer-edit "3rX"
    "XX" "XThis" 'bobp))

(ert-deftest evil-test-insert-before ()
  "Test insertion of text before point"
  :tags '(evil)
  (evil-test-buffer
    (evil-local-mode 1)
    (goto-char (+ 3 (point-min)))
    (should (and (looking-at "This") (looking-back ";; ")))
    (evil-test-macro ("ievil rulz " [escape])
      ";; evil rulz" " This" 'bobp)))

(ert-deftest evil-test-insert-before-with-count ()
  "Test insertion of text before point with repeat count"
  :tags '(evil)
  (evil-test-buffer
    (evil-local-mode 1)
    (goto-char (+ 3 (point-min)))
    (evil-test-text ";; " "This" 'bobp)
    (evil-test-macro ("2ievil rulz " [escape])
      ";; evil rulz evil rulz" " This" 'bobp)))

(ert-deftest evil-test-repeat-insert-before ()
  "Test repeating of insert-before command."
  :tags '(evil)
  (ert-info ("Repeat insert")
    (evil-test-buffer-edit ("iABC" [escape] "..")
      "ABABAB" "CCC;; This"))

  (ert-info ("Repeat insert with count")
    (evil-test-buffer-edit ("2iABC" [escape] "..")
      "ABCABABCABABCAB" "CCC;; This"))

  (ert-info ("Repeat insert with repeat count")
    (evil-test-buffer-edit ("iABC" [escape] "11.")
      "ABABCABCABCABCABCABCABCABCABCABCAB"
      "CC;; This"))

  (ert-info ("Repeat insert with count with repeat with count")
    (evil-test-buffer-edit ("10iABC" [escape] "11.")
      "ABCABCABCABCABCABCABCABCABCABABCABCABCABCABCABCABCABCABCABCAB"
      "CC;; This")))

(ert-deftest evil-test-insert-before-vcount ()
  "Test `evil-insert-before' with vertical repeation."
  :tags '(evil)
  (evil-test-buffer
    (forward-word)
    (define-key evil-normal-state-local-map "i"
      #'(lambda (count)
          (interactive "p")
          (evil-insert-before count 5)))
    (execute-kbd-macro (vconcat "2iABC" [escape]))
    (evil-test-text-lines
     '(";; ThisABCAB" "C buffer" bobp)
     '(";; If yABCAB" "Cou" bolp)
     '(";; thenABCAB" "C enter" bolp)
     '("       ABCAB" "C" bolp eolp)
     '("Below tABCAB" "Che empty" bolp))))

(ert-deftest evil-test-insert-after ()
  "Test insertion of text after point"
  :tags '(evil)
  (evil-test-buffer
    (evil-local-mode 1)
    (goto-char (+ 3 (point-min)))
    (evil-test-text ";; " "This" 'bobp)
    (evil-test-macro ("aevil rulz " [escape])
      ";; Tevil rulz" " his" 'bobp)))

(ert-deftest evil-test-insert-after-with-count ()
  "Test insertion of text after point with repeat count"
  :tags '(evil)
  (evil-test-buffer
    (evil-local-mode 1)
    (goto-char (+ 3 (point-min)))
    (evil-test-macro ("2aevil rulz " [escape])
      ";; Tevil rulz evil rulz" " his" 'bobp)))

(ert-deftest evil-test-repeat-insert-after ()
  "Test repeating of insert-after command."
  :tags '(evil)
  (ert-info ("Repeat insert")
    (evil-test-buffer-edit ("aABC" [escape] "..")
      ";ABCABCAB" "C; This"))

  (ert-info ("Repeat insert with count")
    (evil-test-buffer-edit ("2aABC" [escape] "..")
      ";ABCABCABCABCABCAB" "C; This"))

  (ert-info ("Repeat insert with repeat count")
    (evil-test-buffer-edit ("aABC" [escape] "11.")
      ";ABCABCABCABCABCABCABCABCABCABCABCAB"
      "C; This"))

  (ert-info ("Repeat insert with count with repeat with count")
    (evil-test-buffer-edit ("10aABC" [escape] "11.")
      ";ABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCAB"
      "C; This")))

(ert-deftest evil-test-insert-after-vcount ()
  "Test `evil-insert-after' with vertical repeation."
  :tags '(evil)
  (evil-test-buffer
    (forward-word)
    (define-key evil-normal-state-local-map "a"
      #'(lambda (count)
          (interactive "p")
          (evil-insert-after count 5)))
    (execute-kbd-macro (vconcat "2aABC" [escape]))
    (evil-test-text-lines
     '(";; This ABCAB" "Cbuffer" bobp)
     '(";; If yoABCAB" "Cu" bolp)
     '(";; then ABCAB" "Center" bolp)
     '((lambda () (looking-back "\\(        \\|\t\\)ABCAB")) "C")
     '("Below thABCAB" "Ce empty" bolp))))

(ert-deftest evil-test-insert-above ()
  "Test insertion of text above point"
  :tags '(evil)
  (evil-test-buffer
    (evil-local-mode 1)
    (forward-line)
    (evil-test-macro ("Oabc\ndef" [escape])
      "evaluation.\nabc\nde"
      "f\n;; If you")))

(ert-deftest evil-test-insert-above-with-count ()
  "Test insertion of text above point with repeat count"
  :tags '(evil)
  (evil-test-buffer
    (evil-local-mode 1)
    (forward-line)
    (evil-test-macro ("2Oevil\nrulz" [escape])
      "evaluation.\nevil\nrulz\nevil\nrul"
      "z\n;; If you")))

(ert-deftest evil-test-repeat-insert-above ()
  "Test repeating of insert-above command."
  :tags '(evil)
  (ert-info ("Repeat insert")
    (evil-test-buffer-edit ("Oevil\nrulz" [escape] "..")
      "evil\nevil\nevil\nrul"
      "z\nrulz\nrulz\n;; This"
      'bobp))

  (ert-info ("Repeat insert with count")
    (evil-test-buffer-edit ("2Oevil\nrulz" [escape] "..")
      "evil\nrulz\nevil\nevil\nrulz\nevil\nevil\nrulz\nevil\nrul"
      "z\nrulz\nrulz\n;; This"
      'bobp))

  (ert-info ("Repeat insert with repeat count")
    (evil-test-buffer-edit ("Oevil\nrulz" [escape] "2.")
      "evil\nevil\nrulz\nevil\nrul"
      "z\nrulz\n;; This"))

  (ert-info ("Repeat insert with count with repeat with count")
    (evil-test-buffer-edit ("2Oevil\nrulz" [escape] "3.")
      "evil\nrulz\nevil\nevil\nrulz\nevil\nrulz\nevil\nrul"
      "z\nrulz\n;; This"
      'bobp)))

(ert-deftest evil-test-insert-below ()
  "Test insertion of text below point"
  :tags '(evil)
  (evil-test-buffer
    (evil-local-mode 1)
    (evil-test-macro ("oabc\ndef" [escape])
      "evaluation.\nabc\nde" "f\n;; If you")))

(ert-deftest evil-test-insert-below-with-count ()
  "Test insertion of text below point with repeat count"
  :tags '(evil)
  (evil-test-buffer
    (evil-local-mode 1)
    (evil-test-macro ("2oevil\nrulz" [escape])
      "evaluation.\nevil\nrulz\nevil\nrul" "z\n;; If you")))

(ert-deftest evil-test-repeat-insert-below ()
  "Test repeating of insert-below command."
  :tags '(evil)
  (ert-info ("Repeat insert")
    (evil-test-buffer-edit ("oevil\nrulz" [escape] "..")
      "evaluation.\nevil\nrulz\nevil\nrulz\nevil\nrul"
      "z\n;; If you"))

  (ert-info ("Repeat insert with count")
    (evil-test-buffer-edit ("2oevil\nrulz" [escape] "..")
      "evaluation.\nevil\nrulz\nevil\nrulz\nevil\nrulz\nevil\nrulz\nevil\nrulz\nevil\nrul"
      "z\n;; If you"))

  (ert-info ("Repeat insert with repeat count")
    (evil-test-buffer-edit ("oevil\nrulz" [escape] "2.")
      "evaluation.\nevil\nrulz\nevil\nrulz\nevil\nrul"
      "z\n;; If you"))

  (ert-info ("Repeat insert with count with repeat with count")
    (evil-test-buffer-edit ("2oevil\nrulz" [escape] "3.")
      "evaluation.\nevil\nrulz\nevil\nrulz\nevil\nrulz\nevil\nrulz\nevil\nrul"
      "z\n;; If you")))

(ert-deftest evil-test-insert-beginning-of-line ()
  "Test insertion of text at beginning of line"
  :tags '(evil)
  (evil-test-buffer
    (evil-local-mode 1)
    (goto-char (+ 3 (point-min)))
    (should (and (looking-at "This") (looking-back ";; ")))
    (evil-test-macro ("Ievil rulz " [escape])
      "evil rulz" " ;; This" 'bobp)))

(ert-deftest evil-test-insert-beginning-of-line-with-count ()
  "Test insertion of text at beginning of line with repeat count"
  :tags '(evil)
  (evil-test-buffer
    (evil-local-mode 1)
    (goto-char (+ 3 (point-min)))
    (evil-test-text ";; " "This" 'bobp)
    (evil-test-macro ("2Ievil rulz " [escape])
      "evil rulz evil rulz" " ;; This" 'bobp)))

(ert-deftest evil-test-repeat-insert-beginning-of-line ()
  "Test repeating of insertion at beginning of line."
  :tags '(evil)
  (ert-info ("Repeat insert")
    (evil-test-buffer-edit ("$IABC" [escape] "..")
      "AB" "CABCABC;; This"))

  (ert-info ("Repeat insert with count")
    (evil-test-buffer-edit ("$2IABC" [escape] "..")
      "ABCAB" "CABCABCABCABC;; This"))

  (ert-info ("Repeat insert with repeat count")
    (evil-test-buffer-edit ("$IABC" [escape] "11.")
      "ABCABCABCABCABCABCABCABCABCABCAB" "CABC;; This"))

  (ert-info ("Repeat insert with count with repeat with count")
    (evil-test-buffer-edit ("$10IABC" [escape] "11.")
      "ABCABCABCABCABCABCABCABCABCABCAB" "CABCABCABCABCABCABCABCABCABCABC;; This")))

(ert-deftest evil-test-insert-beginning-of-line-vcount ()
  "Test `evil-insert-beginning-of-line' with vertical repeation."
  :tags '(evil)
  (evil-test-code-buffer
    (forward-line 3)
    (forward-word)
    (define-key evil-normal-state-local-map "I"
      #'(lambda (count)
          (interactive "p")
          (evil-insert-beginning-of-line count 4)))
    (execute-kbd-macro (vconcat "2IABC" [escape]))
    (evil-test-text-lines
     '("ABCAB" "Cint main" bolp)
     '("ABCAB" "C{" bolp eolp)
     '("  ABC" "ABCprintf" bolp)
     '("  ABC" "ABCreturn" bolp))))

(ert-deftest evil-test-insert-end-of-line ()
  "Test insertion of text at end of line"
  :tags '(evil)
  (evil-test-buffer
    (evil-local-mode 1)
    (goto-char (+ 3 (point-min)))
    (evil-test-text ";; " "This" 'bobp)
    (evil-test-macro ("Aevil rulz " [escape])
      "evaluation.evil rulz" " " nil 'eolp)))

(ert-deftest evil-test-insert-end-of-line-with-count ()
  "Test insertion of text at end of line with repeat count"
  :tags '(evil)
  (evil-test-buffer
    (evil-local-mode 1)
    (goto-char (+ 3 (point-min)))
    (evil-test-macro ("2Aevil rulz " [escape])
      "evaluation.evil rulz evil rulz" " " nil 'eolp)))

(ert-deftest evil-test-repeat-insert-end-of-line ()
  "Test repeating of insert-after command."
  :tags '(evil)
  (ert-info ("Repeat insert")
    (evil-test-buffer-edit ("AABC" [escape] "..")
      "evaluation.ABCABCAB" "C" nil 'eolp))

  (ert-info ("Repeat insert with count")
    (evil-test-buffer-edit ("2AABC" [escape] "..")
      "evaluation.ABCABCABCABCABCAB" "C" nil 'eolp))

  (ert-info ("Repeat insert with repeat count")
    (evil-test-buffer-edit ("AABC" [escape] "11.")
      "evaluation.ABCABCABCABCABCABCABCABCABCABCABCAB" "C" nil 'eolp))

  (ert-info ("Repeat insert with count with repeat with count")
    (evil-test-buffer-edit ("10AABC" [escape] "11.")
      "evaluation.ABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCAB"
      "C" nil 'eolp)))

(ert-deftest evil-test-insert-end-of-line-vcount ()
  "Test `evil-insert-end-of-line' with vertical repeation."
  :tags '(evil)
  (evil-test-code-buffer
    (forward-line 3)
    (forward-word)
    (define-key evil-normal-state-local-map "A"
      #'(lambda (count)
          (interactive "p")
          (evil-insert-end-of-line count 4)))
    (execute-kbd-macro (vconcat "2AABC" [escape]))
    (evil-test-text-lines
     '("argv)     ABCAB" "C" nil eolp)
     '("{ABCABC" "" bolp eolp)
     '("world\\n\");ABCABC" "" nil eolp)
     '("EXIT_SUCCESS;ABCABC" "" nil eolp))))

(defun evil-test-dummy-complete ()
  "Test function for change-base repeation.
Removes 5 characters, insert BEGIN\\n\\nEND\\nplaces
cursor on the new line."
  (interactive)
  (delete-char 5)
  (insert "BEGIN\n")
  (save-excursion
    (insert "\nEND\n")))

(ert-deftest evil-test-repeat-by-change ()
  "Test repeation by tracking changes for completion commands."
  :tags '(evil)
  (let (line-move-visual)
    (define-key evil-insert-state-map (kbd "C-c C-p") 'evil-test-dummy-complete)
    (evil-set-insert-repeat-type 'evil-test-dummy-complete 'change)
    (evil-test-buffer
      (forward-char 3)
      (execute-kbd-macro (vconcat "iABC " (kbd "C-c C-p") "BODY" [escape]))
      (forward-line 2)
      (execute-kbd-macro ".")
      (evil-test-text
       ";; ABC BEGIN\nBODY\nEND\nABC BEGIN\nBOD"
       "Y\nEND\nr is for"
       'bobp))))

(ert-deftest evil-test-repeat-kill-buffer ()
  "Test safe-guard preventing buffers from being deleted
when repeating a command."
  :tags '(evil)
  (ert-info ("Test killing works for direct calls
to `evil-execute-repeat-info'")
    (evil-test-buffer
      (evil-local-mode 1)
      (setq evil-repeat-info '((kill-buffer nil)))
      (evil-execute-repeat-info evil-repeat-info)
      (should (not (looking-at ";; This")))))

  (ert-info ("Verify an error is raised when using `evil-repeat' command")
    (evil-test-buffer
      (evil-local-mode 1)
      (setq evil-repeat-info '((kill-buffer nil)))
      (should-error (call-interactively 'evil-repeat)))))

;;; Operators

(ert-deftest evil-test-keypress-parser ()
  "Test `evil-keypress-parser'"
  :tags '(evil)
  (evil-test-buffer
    (evil-test-change-state 'operator)
    (ert-info ("Read from the keyboard unless INPUT is given")
      (let ((unread-command-events '(?d)))
        (should (equal (evil-keypress-parser)
                       '(evil-delete nil)))
        (should (equal (evil-keypress-parser '(?d))
                       '(evil-delete nil)))))
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

(ert-deftest evil-test-operator ()
  "Test operator."
  :tags '(evil)
  (evil-test-buffer-edit ([right right right] "g?" [M-right])
    ";; " "Guvf buffer"))

(ert-deftest evil-test-operator-with-count ()
  "Test operator with count argument."
  :tags '(evil)
  (ert-info ("Count before operator")
    (evil-test-buffer-edit ([right right right] "2g?" [M-right])
      ";; " "Guvf ohssre is"))

  (ert-info ("Count before motion")
    (evil-test-buffer-edit ([right right right] "g?2" [M-right])
      ";; " "Guvf ohssre is"))

  (ert-info ("Count before operator and motion")
    (evil-test-buffer-edit ([right right right] "3g?2" [M-right])
      ";; " "Guvf ohssre vf sbe abgrf lbh don't"))

  (ert-info ("Count exceeding buffer boundaries")
    (evil-test-buffer-edit ([right right right] "g?200" [right])
      ";; " "Guvf ohssre vf sbe abgrf lbh qba'g")))

(ert-deftest evil-test-operator-repeat ()
  "Test repeating of an operator."
  :tags '(evil)
  (evil-test-buffer-edit ([right right right] "g?" [M-right] [M-right] ".")
    ";; Guvf" " ohssre is"))

(ert-deftest evil-test-operator-repeat-with-count ()
  "Test repeating of an operator with new count."
  :tags '(evil)
  (ert-info ("Count before operator")
    (evil-test-buffer-edit ([right right right] "2g?" [M-right] "3.")
      ";; " "This buffer vf for notes"))

  (ert-info ("Count before motion")
    (evil-test-buffer-edit ([right right right] "g?2" [M-right] "3.")
      ";; " "This buffer vf for notes"))

  (ert-info ("Count before operator and motion")
    (evil-test-buffer-edit ([right right right] "3g?2" [M-right] "4.")
      ";; " "This buffer is for abgrf lbh don't")))

(ert-deftest evil-test-operator-delete ()
  "Test deleting text."
  :tags '(evil)
  (ert-info ("Delete characters")
    (evil-test-buffer-edit "dl"
      'bobp "; This buffer is for notes")
    (evil-test-buffer-edit "d1l"
      'bobp "; This buffer is for notes")
    (evil-test-buffer-edit "1dl"
      'bobp "; This buffer is for notes")
    (evil-test-buffer-edit "1d1l"
      'bobp "; This buffer is for notes")
    (evil-test-buffer-edit "d2l"
      'bobp " This buffer is for notes")
    (evil-test-buffer-edit "2dl"
      'bobp " This buffer is for notes")
    (ert-info ("Multiply counts together")
      (evil-test-buffer-edit "d4l"
        'bobp "his buffer is for notes")
      (evil-test-buffer-edit "4dl"
        'bobp "his buffer is for notes")
      (evil-test-buffer-edit "2d2l"
        'bobp "his buffer is for notes")))
  (ert-info ("Delete current line")
    (evil-test-buffer-edit "dd"
      'bobp ";; If you want to create a file")
    (evil-test-buffer-edit "d1d"
      'bobp ";; If you want to create a file")
    (evil-test-buffer-edit "1dd"
      'bobp ";; If you want to create a file")
    (evil-test-buffer-edit "1d1d"
      'bobp ";; If you want to create a file"))
  (ert-info ("Delete two lines")
    (evil-test-buffer-edit "d2d"
      'bobp ";; then enter the text")
    (evil-test-buffer-edit "2dd"
      'bobp ";; then enter the text")
    (evil-test-buffer-edit "dj"
      'bobp ";; then enter the text")
    (evil-test-buffer-edit "jdk"
      'bobp ";; then enter the text")))

(evil-define-motion evil-test-square-motion (count)
  "Test motion, selects a square."
  :type block
  (let ((column (current-column)))
    (forward-line (1- count))
    (move-to-column (+ column count -1))))

(ert-deftest evil-test-yank ()
  "Test yanking of text"
  :tags '(evil)
  (ert-info ("Yank characters")
    (evil-test-buffer
      (execute-kbd-macro "wy2e")
      (should (string= (current-kill 0) "This buffer"))))

  (ert-info ("Yank lines")
    (evil-test-buffer
      (execute-kbd-macro "yj")
      (goto-char (point-min))
      (should
       (string= (current-kill 0)
                (concat (buffer-substring (point-min) (line-end-position 2))
                        "\n")))
      (should (eq (car-safe (get-text-property 0 'yank-handler (current-kill 0)))
                  'evil-yank-line-handler))
      (execute-kbd-macro "jy5j")
      (goto-char (point-min))
      (should
       (string= (current-kill 0)
                (concat (buffer-substring (line-beginning-position 2) (line-end-position 5))
                        "\n")))
      (should (eq (car-safe (get-text-property 0 'yank-handler (current-kill 0)))
                  'evil-yank-line-handler))))

  (ert-info ("Yank rectangle")
    (evil-test-buffer
      (define-key evil-operator-state-local-map "s" 'evil-test-square-motion)
      (execute-kbd-macro "wy3s")
      (goto-char (point-min))
      (should
       (string= (current-kill 0) "Thi\nIf \nthe"))
      (should (eq (car-safe (get-text-property 0 'yank-handler (current-kill 0)))
                  'evil-yank-block-handler)))))

(ert-deftest evil-test-paste-before ()
  "Test `evil-paste-before'"
  :tags '(evil)
  (ert-info ("Paste characters")
    (evil-test-buffer
      (execute-kbd-macro "wy2e^jP")
      (evil-test-text 'bolp "This buffer;; If")))
  (ert-info ("Paste characters with count")
    (evil-test-buffer
      (execute-kbd-macro "wy2e^j3P")
      (evil-test-text 'bolp "This bufferThis bufferThis buffer;; If")))
  (ert-info ("Paste characters at end-of-buffer")
    (evil-test-buffer
      (execute-kbd-macro "wy2eG$2P")
      (evil-test-text "Below the empty line" "This bufferThis buffer." 'bolp 'eobp)))
  (ert-info ("Paste characters at end-of-buffer on empty line.")
    (evil-test-buffer
      (execute-kbd-macro (vconcat "wy2eG$a" (kbd "RET") [escape] "2P"))
      (evil-test-text 'bolp "This bufferThis buffer" nil 'eobp)))

  (ert-info ("Paste lines")
    (evil-test-buffer
      (execute-kbd-macro "2yj4jP")
      (evil-test-text "\n\n" (concat (current-kill 0) "Below the empty line"))))
  (ert-info ("Paste lines with count")
    (evil-test-buffer
      (execute-kbd-macro "2yj4j2P")
      (evil-test-text "\n\n" (concat (current-kill 0) (current-kill 0) "Below the empty line"))))

  (ert-info ("Paste block")
    (evil-test-buffer
      (define-key evil-operator-state-local-map "s" 'evil-test-square-motion)
      (execute-kbd-macro "w3ys^eP")
      (evil-test-text-lines
       '(";" "Thi; This buffer" bobp)
       '(";" "If ; If you" bolp)
       '(";" "the; then enter" bolp))))
  (ert-info ("Paste block with count")
    (evil-test-buffer
      (define-key evil-operator-state-local-map "s" 'evil-test-square-motion)
      (execute-kbd-macro "w3ys^e2P")
      (evil-test-text-lines
       '(";" "ThiThi; This buffer" bobp)
       '(";" "If If ; If you" bolp)
       '(";" "thethe; then enter" bolp))))
  (ert-info ("Paste block with empty line")
    (evil-test-buffer
      (define-key evil-operator-state-local-map "s" 'evil-test-square-motion)
      (execute-kbd-macro "w5ys^e2P")
      (evil-test-text-lines
       '(";" "This This ; This buffer" bobp)
       '(";" "If yoIf yo; If you" bolp)
       '(";" "then then ; then enter" bolp)
       '(bolp eolp)
       '("B" "ow thow thelow the empty" bolp))))
  (ert-info ("Paste block crossing end of buffer")
    (evil-test-buffer
      (define-key evil-operator-state-local-map "s" 'evil-test-square-motion)
      (execute-kbd-macro "w5ys^je2Pk")
      (evil-test-text-lines
       '(";" "; This buffer" bobp)
       '(";" "This This ; If you" bolp)
       '(";" "If yoIf yo; then enter" bolp)
       '(" " "then then" bolp eolp)
       '("B" "          elow the empty" bolp)
       '(" " "ow thow th" bolp eobp))))
  (ert-info ("Paste block at end-of-line")
    (evil-test-buffer
      (define-key evil-operator-state-local-map "s" 'evil-test-square-motion)
      (execute-kbd-macro "w5ysj$2Pk")
      (let ((start-column (current-column)))
        (evil-test-text-lines
         '("for L" "isp evaluation." nil eolp)
         '("C-x C-f" "This This ," nil eolp)
         '((lambda ()
             (and (looking-back "own buffer.\\s-*")
                  (= (current-column) start-column)))
           "If yoIf yo" nil eolp)
         '((lambda ()
             (and (looking-back "^\\s-*")
                  (= (current-column) start-column)))
           "then then" nil eolp)
         '("Below the empty line." eolp)
         '((lambda ()
             (and (looking-back "^\\s-*")
                  (= (current-column) start-column)))
           "ow thow th" nil eolp))))))

(ert-deftest evil-test-paste-behind ()
  "Test `evil-paste-before'"
  :tags '(evil)
  (ert-info ("Paste characters")
    (evil-test-buffer
      (execute-kbd-macro "wy2e^jp")
      (evil-test-text ";This buffe" "r; If" 'bolp)))
  (ert-info ("Paste characters with count")
    (evil-test-buffer
      (execute-kbd-macro "wy2e^j3p")
      (evil-test-text ";This bufferThis bufferThis buffe" "r; If" 'bolp)))
  (ert-info ("Paste characters at end-of-buffer")
    (evil-test-buffer
      (execute-kbd-macro "wy2eG$2p")
      (evil-test-text "Below the empty line.This bufferThis buffe" "r" 'bolp 'eobp)))
  (ert-info ("Paste characters at end-of-buffer on empty line.")
    (evil-test-buffer
      (execute-kbd-macro (vconcat "wy2eG$a" (kbd "RET") [escape] "2p"))
      (evil-test-text "This bufferThis buffe" "r" 'bolp 'eobp)))

  (ert-info ("Paste lines")
    (evil-test-buffer
      (execute-kbd-macro "2yj3jp")
      (evil-test-text "\n\n" (concat (current-kill 0) "Below the empty line"))))
  (ert-info ("Paste lines with count")
    (evil-test-buffer
      (execute-kbd-macro "2yj3j2p")
      (evil-test-text "\n\n" (concat (current-kill 0) (current-kill 0) "Below the empty line"))))
  (ert-info ("Paste lines at end-of-buffer")
    (evil-test-buffer
      (execute-kbd-macro "2yj5j2p")
      (evil-test-text "Below the empty line.\n"
                      (concat (current-kill 0)
                              (substring (current-kill 0) 0 -1))
                      'bolp 'eobp)))

  (ert-info ("Paste block")
    (evil-test-buffer
      (define-key evil-operator-state-local-map "s" 'evil-test-square-motion)
      (execute-kbd-macro "w3ys^ep")
      (evil-test-text-lines
       '(";;" "Thi This buffer" bobp)
       '(";;" "If  If you" bolp)
       '(";;" "the then enter" bolp))))
  (ert-info ("Paste block with count")
    (evil-test-buffer
      (define-key evil-operator-state-local-map "s" 'evil-test-square-motion)
      (execute-kbd-macro "w3ys^e2p")
      (evil-test-text-lines
       '(";;" "ThiThi This buffer" bobp)
       '(";;" "If If  If you" bolp)
       '(";;" "thethe then enter" bolp))))
  (ert-info ("Paste block with empty line")
    (evil-test-buffer
      (define-key evil-operator-state-local-map "s" 'evil-test-square-motion)
      (execute-kbd-macro "w5ys^e2p")
      (evil-test-text-lines
       '(";;" "This This  This buffer" bobp)
       '(";;" "If yoIf yo If you" bolp)
       '(";;" "then then  then enter" bolp)
       '(bolp eolp)
       '("Be" "ow thow thlow the empty" bolp))))
  (ert-info ("Paste block crossing end of buffer")
    (evil-test-buffer
      (define-key evil-operator-state-local-map "s" 'evil-test-square-motion)
      (execute-kbd-macro "w5ys^je2pk")
      (evil-test-text-lines
       '(";;" " This buffer" bobp)
       '(";;" "This This  If you" bolp)
       '(";;" "If yoIf yo then enter" bolp)
       '("  " "then then" bolp eolp)
       '("Be" "          low the empty" bolp)
       '("  " "ow thow th" bolp eobp))))
  (ert-info ("Paste block at end-of-line")
    (evil-test-buffer
      (define-key evil-operator-state-local-map "s" 'evil-test-square-motion)
      (execute-kbd-macro "w5ysj$2pk")
      (let ((start-column (current-column)))
        (evil-test-text-lines
         '("for Li" "sp evaluation." nil eolp)
         '("C-x C-f," "This This" nil eolp)
         '((lambda ()
             (and (looking-back "own buffer\\.\\s-*")
                  (= (current-column) start-column)))
           "If yoIf yo" nil eolp)
         '((lambda ()
             (and (looking-back "^\\s-*")
                  (= (current-column) start-column)))
           "then then" nil eolp)
         '("Below the empty line." eolp)
         '((lambda ()
             (and (looking-back "^\\s-*")
                  (= (current-column) start-column)))
           "ow thow th" nil eolp))))))

(ert-deftest evil-test-paste-pop-before ()
  "Test `evil-paste-pop' after `evil-paste-before'"
  :tags '(evil)
  (evil-test-buffer
    (define-key evil-operator-state-local-map "s" 'evil-test-square-motion)
    (execute-kbd-macro "wy2e2yyy5s^je2P")
    (save-excursion
      (goto-char (1+ (point-min)))
      (evil-test-text-lines
       '(";" "; This buffer" bobp)
       '(";" "This This ; If you" bolp)
       '(";" "If yoIf yo; then enter" bolp)
       '(" " "then then" bolp eolp)
       '("B" "          elow the empty" bolp)
       '(" " "ow thow th" bolp eobp))))

  (evil-test-buffer
    (define-key evil-operator-state-local-map "s" 'evil-test-square-motion)
    (execute-kbd-macro "wy2e2yyy5s^je2P\C-p")
    (evil-test-text "evaluation.\n" (concat (current-kill 0) (current-kill 0) ";; If you")))

  (evil-test-buffer
    (define-key evil-operator-state-local-map "s" 'evil-test-square-motion)
    (execute-kbd-macro "wy2e2yyy5s^je2P\C-p\C-p")
    (evil-test-text "evaluation.\n;" "This bufferThis buffer; If you"))

  (evil-test-buffer
    (define-key evil-operator-state-local-map "s" 'evil-test-square-motion)
    (execute-kbd-macro "wy2e2yyy5s^je2P2\C-p")
    (evil-test-text "evaluation.\n;" "This bufferThis buffer; If you"))

  (evil-test-buffer
    (define-key evil-operator-state-local-map "s" 'evil-test-square-motion)
    (execute-kbd-macro "wy2e2yyy5s^je2P2\C-p\C-n")
    (evil-test-text "evaluation.\n" (concat (current-kill 0) (current-kill 0) ";; If you")))

  (evil-test-buffer
    (define-key evil-operator-state-local-map "s" 'evil-test-square-motion)
    (execute-kbd-macro "wy2e2yyy5s^je2P\C-p\C-p2\C-n")
    (save-excursion
      (goto-char (1+ (point-min)))
      (evil-test-text-lines
       '(";" "; This buffer" bobp)
       '(";" "This This ; If you" bolp)
       '(";" "If yoIf yo; then enter" bolp)
       '(" " "then then" bolp eolp)
       '("B" "          elow the empty" bolp)
       '(" " "ow thow th" bolp eobp)))))

(ert-deftest evil-test-paste-pop-behind ()
  "Test `evil-paste-pop' after `evil-paste-behind'"
  :tags '(evil)
  (evil-test-buffer
    (define-key evil-operator-state-local-map "s" 'evil-test-square-motion)
    (execute-kbd-macro "wy2e2yyy5s^je2p")
    (save-excursion
      (goto-char (+ 2 (point-min)))
      (evil-test-text-lines
       '(";;" " This buffer" bobp)
       '(";;" "This This  If you" bolp)
       '(";;" "If yoIf yo then enter" bolp)
       '("  " "then then" bolp eolp)
       '("Be" "          low the empty" bolp)
       '("  " "ow thow th" bolp eobp))))

  (evil-test-buffer
    (define-key evil-operator-state-local-map "s" 'evil-test-square-motion)
    (execute-kbd-macro "wy2e2yyy5s^je2p\C-p")
    (evil-test-text "with C-x C-f,\n" (concat (current-kill 0) (current-kill 0) ";; then enter")))

  (evil-test-buffer
    (define-key evil-operator-state-local-map "s" 'evil-test-square-motion)
    (execute-kbd-macro "wy2e2yyy5s^je2p\C-p\C-p")
    (evil-test-text "evaluation.\n;;This bufferThis buffe" "r If you"))

  (evil-test-buffer
    (define-key evil-operator-state-local-map "s" 'evil-test-square-motion)
    (execute-kbd-macro "wy2e2yyy5s^je2p2\C-p")
    (evil-test-text "evaluation.\n;;This bufferThis buffe" "r If you"))

  (evil-test-buffer
    (define-key evil-operator-state-local-map "s" 'evil-test-square-motion)
    (execute-kbd-macro "wy2e2yyy5s^je2p2\C-p\C-n")
    (evil-test-text "with C-x C-f,\n" (concat (current-kill 0) (current-kill 0) ";; then enter")))

  (evil-test-buffer
    (define-key evil-operator-state-local-map "s" 'evil-test-square-motion)
    (execute-kbd-macro "wy2e2yyy5s^je2p\C-p\C-p2\C-n")
    (save-excursion
      (goto-char (+ 2 (point-min)))
      (evil-test-text-lines
       '(";;" " This buffer" bobp)
       '(";;" "This This  If you" bolp)
       '(";;" "If yoIf yo then enter" bolp)
       '("  " "then then" bolp eolp)
       '("Be" "          low the empty" bolp)
       '("  " "ow thow th" bolp eobp)))))

(ert-deftest evil-test-delete ()
  "Test `evil-delete'"
  :tags '(evil)
  (ert-info ("Delete characters")
    (evil-test-buffer
      (evil-test-macro "wd2e" ";; " " is for" 'bobp)
      (should (string= (current-kill 0) "This buffer"))
      (evil-test-macro "P" ";; " "This buffer is for" 'bobp)))

  (ert-info ("Delete lines")
    (evil-test-buffer
      (evil-test-macro "2dd" 'bobp ";; then enter")
      (evil-test-macro "P" 'bobp ";; This buffer")))

  (ert-info ("Delete last line")
    (evil-test-buffer
      (evil-test-macro "Gk2dd" "buffer" "." nil 'eobp)))

  (ert-info ("Delete rectangle")
    (evil-test-buffer
      (define-key evil-operator-state-local-map "s" 'evil-test-square-motion)
      (execute-kbd-macro "wd3s")
      (evil-test-text-lines
       '(";; " "s buffer" bobp)
       '(";; " "you want" bolp)
       '(";; " "n enter" bolp)
       '(bolp eolp)))))

(ert-deftest evil-test-change ()
  "Test `evil-change'"
  :tags '(evil)
  (ert-info ("Change characters")
    (evil-test-buffer
      (execute-kbd-macro (vconcat "wc2eABC" [escape]))
      (evil-test-text ";; AB" "C is for" 'bobp)
      (should (string= (current-kill 0) "This buffer"))
      (evil-test-macro "p" ";; ABCThis buffe" "r is for" 'bobp)))

  (ert-info ("Change lines")
    (evil-test-buffer
      (execute-kbd-macro (vconcat "2ccABCLINE\nDEFLINE" [escape]))
      (evil-test-text "ABCLINE\nDEFLIN" "E\n;; then enter" 'bobp)
      (evil-test-macro "p" "DEFLINE\n" ";; This buffer")))

  (ert-info ("Change last line")
    (evil-test-buffer
      (execute-kbd-macro (vconcat "Gk2ccABC" [escape]))
      (evil-test-text "buffer.\nAB" "C" nil 'eobp)))

  (ert-info ("Change rectangle")
    (evil-test-buffer
      (define-key evil-operator-state-local-map "s" 'evil-test-square-motion)
      (execute-kbd-macro (vconcat "wc3sABC" [escape]))
      (evil-test-text-lines
       '(";; AB" "Cs buffer" bobp)
       '(";; AB" "Cyou want" bolp)
       '(";; AB" "Cn enter" bolp)
       '(bolp eolp)))))

(ert-deftest evil-change-word ()
  "Test change of word."
  (ert-info ("Non-word")
    (evil-test-buffer
      (execute-kbd-macro (vconcat "cwABC" [escape]))
      (evil-test-text "AB" "C This buffer" 'bobp)))
  (ert-info ("Word")
    (evil-test-buffer
      (execute-kbd-macro (vconcat "wcwABC" [escape]))
      (evil-test-text ";; AB" "C buffer" 'bobp)))
  (ert-info ("Single character")
    (evil-test-buffer
      (delete-char 1)
      (execute-kbd-macro (vconcat "cwABC" [escape]))
      (evil-test-text "AB" "C This buffer" 'bobp))))

(ert-deftest evil-join-lines ()
  "Test `evil-join-lines'."
  (ert-info ("Simple")
    (evil-test-buffer-edit "J"
      "evaluation." " ;; If you"))

  (ert-info ("Visual")
    (evil-test-buffer-edit "VjJ"
      "evaluation." " ;; If you")))


;;; Motions

(ert-deftest evil-test-forward-char ()
  "Test `evil-forward-char' motion."
  :tags '(evil)
  (ert-info ("Simple")
    (evil-test-buffer-edit "l"
      ";" "; This" 'bobp))
  (ert-info ("With count")
    (evil-test-buffer-edit "12l"
      ";; This buff" "er is" 'bobp))
  (ert-info ("End of line")
    (evil-test-buffer
      (end-of-line)
      (backward-char)
      (should-error (execute-kbd-macro "l"))
      (should-error (execute-kbd-macro "10l"))))
  (ert-info ("Until end-of-line")
    (evil-test-buffer-edit "100l"
      "evaluation" ".\n"))
  (ert-info ("On empty line")
    (evil-test-buffer
      (evil-test-macro (forward-line 3)
        "buffer.\n" "\nBelow")
      (should-error (execute-kbd-macro "l"))
      (evil-test-text "buffer.\n" "\nBelow")
      (should-error (execute-kbd-macro "42l"))
      (evil-test-text "buffer.\n" "\nBelow"))))

(ert-deftest evil-test-backward-char ()
  "Test `evil-backward-char' motion."
  :tags '(evil)
  (ert-info ("Simple")
    (evil-test-buffer
      (evil-test-macro (forward-word)
        "This" " buffer")
      (evil-test-macro "h"
        ";; Thi" "s buffer" 'bobp)))
  (ert-info ("With count")
    (evil-test-buffer
      (evil-test-macro (forward-word)
        "This" " buffer")
      (evil-test-macro "3h"
        ";; T" "his buffer" 'bobp)
      (evil-test-macro "100h"
        'bobp ";; This buffer")))
  (ert-info ("Beginning of line")
    (evil-test-buffer
      (forward-line)
      (should-error (execute-kbd-macro "h"))
      (evil-test-text "\n" ";; If you")
      (should-error (execute-kbd-macro "10h"))
      (evil-test-text "\n" ";; If you")))
  (ert-info ("Until beginning-of-line")
    (evil-test-buffer
      (forward-line)
      (forward-word)
      (evil-test-text ";; If" " you")
      (evil-test-macro "100h"
        "\n" ";; If you")))
  (ert-info ("On empty line")
    (evil-test-buffer
      (evil-test-macro (forward-line 3)
        "buffer.\n" "\nBelow")
      (should-error (execute-kbd-macro "h"))
      (evil-test-text "buffer.\n" "\nBelow")
      (should-error (execute-kbd-macro "42h"))
      (evil-test-text "buffer.\n" "\nBelow"))))

(ert-deftest evil-test-previous-line ()
  "Test `evil-previous-line' motion."
  :tags '(evil)
  (ert-info ("Simple")
    (evil-test-buffer
      (forward-line 4)
      (forward-word)
      (evil-test-text "\nBelow" " the")
      (evil-test-macro "k"
        "own buffer.\n" "\nBelow")))
  (ert-info ("With count")
    (evil-test-buffer
      (forward-line 4)
      (forward-word)
      (evil-test-text "\nBelow" " the")
      (evil-test-macro "2k"
        ";; th" "en enter")))
  (ert-info ("Until beginning of buffer")
    (evil-test-buffer
      (forward-line 4)
      (forward-word)
      (evil-test-text "\nBelow" " the")
      (evil-test-macro "100k"
        ";; Th" "is buffer")))
  (ert-info ("At beginning of buffer")
    (evil-test-buffer
      (evil-test-macro (forward-word)
        ";; This" " buffer")
      (should-error (execute-kbd-macro "k"))
      (evil-test-text ";; This" " buffer")
      (should-error (execute-kbd-macro "42k"))
      (evil-test-text ";; This" " buffer"))))

(ert-deftest evil-test-next-line ()
  "Test `evil-next-line' motion."
  :tags '(evil)
  (ert-info ("Simple")
    (evil-test-buffer
      (evil-test-macro (forward-word)
        ";; This" " buffer")
      (evil-test-macro "j"
        ";; If y" "ou")))
  (ert-info ("With count")
    (evil-test-buffer
      (evil-test-macro (forward-word)
        ";; This" " buffer")
      (evil-test-macro "2j"
        ";; then" " enter")))
  (ert-info ("Until end of buffer")
    (evil-test-buffer
      (evil-test-macro (forward-word)
        ";; This" " buffer")
      (evil-test-macro "100j"
        "Below t" "he ")))
  (ert-info ("At end of buffer")
    (evil-test-buffer
      (re-search-forward "Below")
      (evil-test-text "\nBelow" " the")
      (should-error (execute-kbd-macro "j"))
      (evil-test-text "\nBelow" " the")
      (should-error (execute-kbd-macro "42j"))
      (evil-test-text "\nBelow" " the"))))

(ert-deftest evil-test-beginning-of-line ()
  "Test `evil-beginning-line' motion."
  :tags '(evil)
  (evil-test-buffer
    (forward-line)
    (forward-word)
    (evil-test-text ";; If" " you")
    (dotimes (i 2)
      (evil-test-macro "0"
        "evaluation\.\n" ";; If you"))))

(ert-deftest evil-test-end-of-line ()
  "Test `evil-end-line' motion."
  :tags '(evil)
  (evil-test-buffer
    (forward-line)
    (forward-word)
    (evil-test-text ";; If" " you")
    (dotimes (i 2)
      (evil-test-macro "$"
        "C-x C-f" ",\n;; then"))
    (evil-test-macro (forward-line 2)
      "buffer.\n" "\nBelow")
    (evil-test-macro "$"
      "buffer.\n" "\nBelow")))

(ert-deftest evil-test-first-non-blank ()
  "Test `evil-first-non-blank' motion."
  :tags '(evil)
  (evil-test-code-buffer
    (forward-line 5)
    (end-of-line)
    (backward-char)
    (evil-test-text "world\\n\")" ";\n  return")
    (dotimes (i 2)
      (evil-test-macro "^"
        "{\n  " "printf"))
    (evil-test-macro (forward-line 2)
      "SUCCESS;\n" "     \n}")
    (evil-test-macro "^"
      "SUCCESS;\n    " " \n}")))

(ert-deftest evil-test-last-non-blank ()
  "Test `evil-last-non-blank' motion."
  :tags '(evil)
  (evil-test-code-buffer
    (evil-test-macro (forward-line 3)
      "\n" "int main")
    (dotimes (i 2)
      (evil-test-macro "g_"
        "argv" ")     \n"))
    (forward-line 4)
    (forward-char 3)
    (evil-test-text "SUCCESS;\n   " "  \n}")
    (evil-test-macro "g_"
      "SUCCESS;\n" "     \n}")))

(ert-deftest evil-test-first-non-blank-beg ()
  "Test `evil-first-non-blank-beg' motion."
  :tags '(evil)
  (evil-test-code-buffer
    (evil-test-macro "6gg"
      "{\n  " "printf")
    (evil-test-macro "3gg"
      "stdlib.h>\n" "\nint")
    (evil-test-macro "8gg"
      "SUCCESS;\n    " " \n}")
    (evil-test-macro "gg"
      'bobp "#include <stdio.h>"))
  (evil-test-buffer
    (evil-test-macro "100gg"
      "\n\n" "Below the empty line.")))

(ert-deftest evil-test-first-non-blank-end ()
  "Test `evil-first-non-blank-beg' motion."
  :tags '(evil)
  (evil-test-code-buffer
    (evil-test-macro "6G"
      "{\n  " "printf")
    (evil-test-macro "3G"
      "stdlib.h>\n" "\nint")
    (evil-test-macro "8G"
      "SUCCESS;\n    " " \n}")
    (evil-test-macro "G"
      "}\n" 'eobp))
  (evil-test-buffer
    (evil-test-macro "G"
      "\n\n" "Below the empty line.")
    (goto-char (point-min))
    (evil-test-macro "100G"
      "\n\n" "Below the empty line.")))

(ert-deftest evil-test-operator-0 ()
  "Test motion \"0\" with an operator."
  :tags '(evil))

;; TODO: I don't know how to test the visual motions or window motions.

(ert-deftest evil-test-move-chars ()
  "Test `evil-test-move-chars'."
  :tags '(evil)
  (evil-test-code-buffer
    (ert-info ("Simple forward")
      (evil-move-chars "{" 1)
      (evil-test-text "argv)     \n{" "")
      (evil-move-chars "a-z" 1)
      (evil-test-text "printf" "")
      (evil-move-chars "a-z" 1)
      (evil-test-text "Hello" " world"))
    (ert-info ("End of buffer")
      (should (= 1 (evil-move-chars "Q" 1))))
    (ert-info ("Simple backward")
      (evil-move-chars "*" -1)
      (evil-test-text "char" "** argv)")
      (evil-move-chars "*" -1)
      (evil-test-text "char" "** argv)"))
    (ert-info ("Beginning of buffer")
      (should (= -1 (evil-move-chars "Q" -1))))))

(ert-deftest evil-test-forward-word-end ()
  "Test `evil-test-forward-word-end'"
  :tags '(evil)
  (evil-test-buffer
    (ert-info ("Non-word")
      (evil-test-macro "e" ";" "; This" 'bobp))
    (ert-info ("Word")
      (evil-test-macro "e" ";; Thi" "s buffer" 'bobp))
    (ert-info ("With count")
      (evil-test-macro "3e" ";; This buffer is fo" "r" 'bobp))
    (ert-info ("With count on whitespace")
      (backward-word)
      (backward-char)
      (evil-test-text "buffer is" " for")
      (evil-test-macro "2e" "is for note" "s"))
    (ert-info ("Empty line")
      (evil-test-macro "47e" "buffer.\n" "\nBelow"))
    (ert-info ("End of buffer")
      (evil-test-macro "1000e" "empty line" ".")
      (should-error (execute-kbd-macro "e"))
      (should-error (execute-kbd-macro "10e")))))

(ert-deftest evil-test-forward-word-begin ()
  "Test `evil-test-forward-word-begin'"
  :tags '(evil)
  (evil-test-buffer
    (ert-info ("Simple")
      (evil-test-macro "w" ";; " "This"))
    (ert-info ("With count")
      (evil-test-macro "6w" "you " "don"))
    (ert-info ("Non-word")
      (evil-test-macro "w" "don" "'t")
      (evil-test-macro "11w" "evaluation.\n" ";; If"))
    (ert-info ("On whitespace")
      (forward-word)
      (evil-test-text ";; If" " you")
      (evil-test-macro "2w" "evaluation.\n;; If you " "want"))
    (ert-info ("Empty line")
      (evil-test-macro "30w" "buffer.\n" "\nBelow"))
    (ert-info ("End of buffer")
      (evil-test-macro "1000w" "line" ".")
      (should-error (execute-kbd-macro "w"))
      (should-error (execute-kbd-macro "3w")))))

(ert-deftest evil-test-backward-word-end ()
  "Test `evil-test-backward-word-end'"
  :tags '(evil)
  (evil-test-buffer
    (goto-char (1- (point-max)))
    (ert-info ("Simple")
      (evil-test-macro "ge" "lin" "e."))
    (ert-info ("With count")
      (evil-test-macro "2ge" "Below th" "e empty"))
    (ert-info ("Empty line")
      (evil-test-macro "2ge" "buffer.\n" "\nBelow"))
    (ert-info ("With count on whitespace")
      (backward-word)
      (backward-char)
      (evil-test-text "own" " buffer")
      (evil-test-macro "2ge" "file'" "s own"))
    (ert-info ("Beginning of buffer")
      (evil-test-macro "1000ge" 'bobp ";; This")
      (should-error (execute-kbd-macro "ge"))
      (should-error (execute-kbd-macro "10ge")))))

(ert-deftest evil-test-backward-word-begin ()
  "Test `evil-test-backward-word-begin'"
  :tags '(evil)
  (evil-test-buffer
    (goto-char (1- (point-max)))
    (ert-info ("Simple")
      (evil-test-macro "b" "the empty " "line."))
    (ert-info ("With count")
      (evil-test-macro "2b" "Below " "the empty"))
    (ert-info ("Empty line")
      (evil-test-macro "2b" "buffer.\n" "\nBelow"))
    (ert-info ("With count on whitespace")
      (backward-word)
      (backward-char)
      (evil-test-text "own" " buffer")
      (evil-test-macro "4b" "" "file's own"))
    (ert-info ("Beginning of buffer")
      (evil-test-macro "1000b" 'bobp ";; This")
      (should-error (execute-kbd-macro "b"))
      (should-error (execute-kbd-macro "10b")))))

(ert-deftest evil-test-move-paragraph ()
  "Test `evil-move-paragraph'"
  :tags '(evil)
  (evil-test-paragraph-buffer
    (ert-info ("Simple forward")
      (should (= (evil-move-paragraph 1) 0))
      (evil-test-text "own buffer." 'eolp)
      (should (= (evil-move-paragraph 1) 0))
      (evil-test-text "Single Line" 'eolp))
    (goto-char (point-min))
    (ert-info ("Forward with count")
      (should (= (evil-move-paragraph 2) 0))
      (evil-test-text "Single Line" 'eolp))
    (ert-info ("End of buffer without newline")
      (should (= (evil-move-paragraph 2) 1))
      (evil-test-text "own buffer." 'evil-eobp)
      (should (= (evil-move-paragraph 2) 2))
      (evil-test-text "own buffer." 'evil-eobp)
      (should (= (evil-move-paragraph 1) 1))
      (evil-test-text "own buffer." 'evil-eobp)))
  (evil-test-paragraph-buffer :end-newlines 2
    (ert-info ("End of buffer with newline")
      (should (= (evil-move-paragraph 4) 1))
      (evil-test-text "own buffer." "\n\n" nil 'eobp)
      (should (= (evil-move-paragraph 2) 2))
      (evil-test-text "own buffer." "\n\n" nil 'eobp)
      (should (= (evil-move-paragraph 1) 1))
      (evil-test-text "own buffer." '"\n\n" nil 'eobp)))
  (evil-test-paragraph-buffer
    (goto-char (1- (point-max)))
    (ert-info ("Simple backward")
      (should (= (evil-move-paragraph -1) 0))
      (evil-test-text 'bolp ";; This buffer")
      (should (= (evil-move-paragraph -1) 0))
      (evil-test-text 'bolp "Single Line"))
    (goto-char (1- (point-max)))
    (ert-info ("Backward with count")
      (should (= (evil-move-paragraph -2) 0))
      (evil-test-text 'bolp "Single Line"))
    (ert-info ("Beginning of buffer without newline")
      (should (= (evil-move-paragraph -2) -1))
      (evil-test-text 'bobp ";; This buffer")
      (should (= (evil-move-paragraph -2) -2))
      (evil-test-text 'bobp ";; This buffer")
      (should (= (evil-move-paragraph -1) -1))
      (evil-test-text 'bobp ";; This buffer")))
  (evil-test-paragraph-buffer :begin-newlines 2
    (goto-char (1- (point-max)))
    (ert-info ("Beginning of buffer with newline")
      (should (= (evil-move-paragraph -4) -1))
      (evil-test-text "\n\n" ";; This buffer" 'bobp)
      (should (= (evil-move-paragraph -2) -2))
      (evil-test-text "\n\n" ";; This buffer" 'bobp)
      (should (= (evil-move-paragraph -1) -1))
      (evil-test-text "\n\n" ";; This buffer" 'bobp))))

(ert-deftest evil-test-forward-paragraph ()
  "Test `evil-test-forward-paragraph'"
  :tags '(evil)
  (ert-info ("Simple")
    (evil-test-paragraph-buffer
      (evil-test-macro "}" "own buffer.\n" 'bolp)))
  (ert-info ("With count")
    (evil-test-paragraph-buffer
      (evil-test-macro "2}" "Single Line\n" 'bolp)))
  (ert-info ("End of buffer")
    (evil-test-paragraph-buffer
      (evil-test-macro "100}" "own buffer" "." nil 'evil-eobp)
      (should-error (execute-kbd-macro "}"))
      (should-error (execute-kbd-macro "42}"))))
  (ert-info ("End of buffer with newline")
    (evil-test-paragraph-buffer :end-newlines 2
      (evil-test-macro "100}" "own buffer.\n\n" 'evil-eobp)
      (should-error (execute-kbd-macro "}"))
      (should-error (execute-kbd-macro "42}")))))

(ert-deftest evil-test-backward-paragraph ()
  "Test `evil-test-backward-paragraph'"
  :tags '(evil)
  (ert-info ("Simple")
    (evil-test-paragraph-buffer
      (goto-char (1- (point-max)))
      (evil-test-macro "{" 'bolp "\n;; This buffer")))
  (ert-info ("With count")
    (evil-test-paragraph-buffer
      (goto-char (1- (point-max)))
      (evil-test-macro "2{" 'bolp "\nSingle Line")))
  (ert-info ("Beginning of buffer")
    (evil-test-paragraph-buffer
      (goto-char (1- (point-max)))
      (evil-test-macro "100{" 'bobp ";; This")
      (should-error (execute-kbd-macro "{"))
      (should-error (execute-kbd-macro "42{"))))
  (ert-info ("Beginning of buffer with newlines")
    (evil-test-paragraph-buffer :begin-newlines 2
      (goto-char (1- (point-max)))
      (evil-test-macro "100{" 'bobp "\n\n;; This")
      (should-error (execute-kbd-macro "{"))
      (should-error (execute-kbd-macro "42{")))))

(ert-deftest evil-test-forward-sentence ()
  "Test `evil-test-forward-sentence'"
  :tags '(evil)
  (ert-info ("Simple")
    (evil-test-paragraph-buffer
      (evil-test-macro ")" 'bolp  ";; If you")
      (evil-test-macro ")" "own buffer.\n" 'bolp)
      (evil-test-macro ")" 'bolp "Single Line")))
  (ert-info ("With count")
    (evil-test-paragraph-buffer
      (evil-test-macro "2)" "own buffer.\n" 'bolp)
      (evil-test-macro "2)" "Single Line\n" 'bolp)
      (evil-test-macro "2)" 'bolp ";; If you want")))
  (ert-info ("End of buffer")
    (evil-test-paragraph-buffer
      (evil-test-macro "100)" "own buffer" "." nil 'evil-eobp)
      (should-error (execute-kbd-macro ")"))
      (should-error (execute-kbd-macro "42)"))))
  (ert-info ("End of buffer with newline")
    (evil-test-paragraph-buffer :begin-newlines 2 :end-newlines 2
      (evil-test-macro "8)" "own buffer.\n" 'bolp)
      (evil-test-macro "100)" "own buffer.\n\n" 'evil-eobp)
      (should-error (execute-kbd-macro ")"))
      (should-error (execute-kbd-macro "42)")))))

(ert-deftest evil-test-backward-sentence ()
  "Test `evil-test-backward-sentence'"
  :tags '(evil)
  (ert-info ("Simple")
    (evil-test-paragraph-buffer
      (goto-char (1- (point-max)))
      (evil-test-macro "(" 'bolp ";; If you")
      (evil-test-macro "(" 'bolp ";; This buffer")
      (evil-test-macro "(" 'bolp "\n;; This buffer")))
  (ert-info ("With count")
    (evil-test-paragraph-buffer
      (goto-char (1- (point-max)))
      (evil-test-macro "2(" 'bolp ";; This buffer")
      (evil-test-macro "2(" 'bolp "Single Line")))
  (ert-info ("Beginning of buffer")
    (evil-test-paragraph-buffer
      (goto-char (1- (point-max)))
      (evil-test-macro "100(" 'bobp ";; This")
      (should-error (execute-kbd-macro "("))
      (should-error (execute-kbd-macro "42("))))
  (ert-info ("Beginning of buffer with newlines")
    (evil-test-paragraph-buffer :begin-newlines 2
      (goto-char (1- (point-max)))
      (evil-test-macro "7(" "\n\n" ";; This" 'bobp)
      (evil-test-macro "(" "\n" "\n;; This" 'bobp)
      (evil-test-macro "100(" 'bobp "\n\n;; This")
      (should-error (execute-kbd-macro "("))
      (should-error (execute-kbd-macro "42(")))))

(ert-deftest evil-test-find-char ()
  "Test `evil-find-char'."
  :tags '(evil)
  (ert-info ("Simple")
    (evil-test-buffer-edit "fT" ";; " "This buffer" 'bobp))
  (ert-info ("With count")
    (evil-test-buffer-edit "2fe" ";; This buffer is for not" "es you" 'bobp))
  (ert-info ("Repeat")
    (evil-test-buffer-edit "fe;" ";; This buffer is for not" "es you" 'bobp))
  (ert-info ("Repeat backward")
    (evil-test-buffer-edit "2fe," ";; This buff" "er is for notes you" 'bobp))
  (ert-info ("End of line")
    (let (evil-find-skip-newlines)
      (evil-test-buffer
        (should-error (execute-kbd-macro "fI"))
        (evil-test-text 'bobp ";; This")))
    (let ((evil-find-skip-newlines t))
      (evil-test-buffer-edit "fI" ";; " "If you" 'bolp))))

(ert-deftest evil-test-find-char-to ()
  "Test `evil-find-char'."
  :tags '(evil)
  (ert-info ("Simple")
    (evil-test-buffer-edit "tT" ";;" " This buffer" 'bobp))
  (ert-info ("With count")
    (evil-test-buffer-edit "2te" ";; This buffer is for no" "tes you" 'bobp))
  (ert-info ("Repeat")
    (evil-test-buffer-edit "2te;" ";; This buffer is for no" "tes you" 'bobp)
    (evil-test-buffer-edit "2te2;" "don't want to sa" "ve, and"))
  (ert-info ("Repeat backward")
    (evil-test-buffer-edit "2te," ";; This buffe" "r is for notes you" 'bobp))
  (ert-info ("End of line")
    (let (evil-find-skip-newlines)
      (evil-test-buffer
        (should-error (execute-kbd-macro "tI"))
        (evil-test-text 'bobp ";; This")))
    (let ((evil-find-skip-newlines t))
      (evil-test-buffer-edit "tI" ";;" " If you" 'bolp))))

(ert-deftest evil-test-find-char-backward ()
  "Test `evil-find-char'."
  :tags '(evil)
  (ert-info ("Simple")
    (evil-test-buffer-edit "$FT" ";; " "This buffer" 'bobp))
  (ert-info ("With count")
    (evil-test-buffer-edit "$2Fe" "to sav" "e, and"))
  (ert-info ("Repeat")
    (evil-test-buffer-edit "$Fe;" "to sav" "e, and"))
  (ert-info ("Repeat backward")
    (evil-test-buffer-edit "$2Fe," "Lisp " "evaluation." nil 'eolp))
  (ert-info ("Beginning of line")
    (let (evil-find-skip-newlines)
      (evil-test-buffer
        (should-error (execute-kbd-macro "jwFT"))
        (evil-test-text ";; " "If you" 'bolp)))
    (let ((evil-find-skip-newlines t))
      (evil-test-buffer-edit "jwFT" ";; " "This buffer" 'bobp))))

(ert-deftest evil-test-find-char-to-backward ()
  "Test `evil-find-char'."
  :tags '(evil)
  (ert-info ("Simple")
    (evil-test-buffer-edit "$TT" ";; T" "his buffer" 'bobp))
  (ert-info ("With count")
    (evil-test-buffer-edit "$2Te" "to save" ", and"))
  (ert-info ("Repeat")
    (evil-test-buffer-edit "$Te;" "Lisp e" "valuation." nil 'eolp)
    (evil-test-buffer-edit "$Te2;" "to save" ", and"))
  (ert-info ("Repeat backward")
    (evil-test-buffer-edit "$2Te," "Lisp" " evaluation." nil 'eolp))
  (ert-info ("Beginning of line")
    (let (evil-find-skip-newlines)
      (evil-test-buffer
        (should-error (execute-kbd-macro "jwTT"))
        (evil-test-text ";; " "If you" 'bolp)))
    (let ((evil-find-skip-newlines t))
      (evil-test-buffer-edit "jwTT" ";; T" "his buffer" 'bobp))))

(ert-deftest evil-test-jump-item ()
  "Test `evil-jump-item'."
  (ert-info ("Simple")
    (evil-test-code-buffer
      (forward-line 3)
      (re-search-forward "(" nil t)
      (backward-char)
      (evil-test-text "main" "(int argc")
      (evil-test-macro "%" "argv" ")")
      (evil-test-macro "%" "main" "(int argc")))
  (ert-info ("Before parenthesis")
    (evil-test-code-buffer
      (forward-line 3)
      (evil-test-macro "%" "argv" ")")
      (backward-char 5)
      (evil-test-macro "%" "main" "(int argc")))
  (ert-info ("Over several lines")
    (evil-test-code-buffer
      (forward-line 4)
      (evil-test-macro "%" "EXIT_SUCCESS;\n     \n" "}" nil 'eolp)))
  (ert-info ("On line without parenthesis")
    (evil-test-buffer
      (should-error (execute-kbd-macro "%")))))

;;; Visual state

(defun evil-test-visual-select (type &optional mark point)
  "Verify that TYPE is selected correctly"
  (evil-visual-select mark point type)
  (ert-info ("Activate region unless TYPE is `block'")
    (cond
     ((eq type 'block)
      (should (mark t))
      (should-not (region-active-p))
      (should-not transient-mark-mode))
     (t
      (should (mark))
      (should (region-active-p)))))
  (ert-info ("Refresh `evil-visual-overlay'")
    (should (overlayp evil-visual-overlay))
    (should (= (overlay-start evil-visual-overlay)
               (car (evil-expand (point) (mark) type))))
    (should (= (overlay-end evil-visual-overlay)
               (cadr (evil-expand (point) (mark) type))))
    (should (eq (evil-type evil-visual-overlay) type))
    (should (eq (overlay-get evil-visual-overlay 'direction)
                (if (< (point) (mark)) -1 1)))
    (should (eq (overlay-get evil-visual-overlay :expanded) t)))
  (ert-info ("Use `evil-visual-overlay' for highlighting
unless TYPE is `block'")
    (cond
     ((eq type 'block)
      (should evil-visual-block-overlays)
      (should-not (overlay-get evil-visual-overlay 'face)))
     (t
      (should-not evil-visual-block-overlays)
      (should (overlay-get evil-visual-overlay 'face))))))

(ert-deftest evil-test-visual-char ()
  "Test Visual character selection"
  :tags '(evil)
  (evil-test-buffer
    (evil-test-visual-select evil-visual-char))
  (ert-info ("Move to other end")
    (evil-test-buffer
      (evil-test-macro "wve"
        "Thi" "s buffer")
      (evil-test-macro "o"
        ";; " "This buffer")))
  (ert-info ("Delete a word")
    (evil-test-buffer-edit "wved"
      ";; " " buffer" 'bobp))
  (ert-info ("Delete a line")
    (evil-test-buffer-edit "wvjeVovd"
      ";; " " you" 'bobp)))

(ert-deftest evil-test-visual-line ()
  "Test Visual line selection"
  :tags '(evil)
  (evil-test-buffer
    (evil-test-visual-select evil-visual-line))
  (ert-info ("Move to other end")
    (evil-test-buffer
      (evil-test-macro "wVe"
        "Thi" "s buffer")
      (evil-test-macro "o"
        ";; " "This buffer")))
  (ert-info ("Delete a line")
    (evil-test-buffer-edit "wVed"
      'bobp ";; If you want to create a file"))
  (ert-info ("Delete two lines")
    (evil-test-buffer-edit "wVjevoVd"
      'bobp ";; then enter the text in that file")))

(ert-deftest evil-test-visual-block ()
  "Test Visual line selection"
  :tags '(evil)
  (evil-test-buffer
    (evil-test-visual-select evil-visual-block))
  (ert-info ("Move to other corner")
    (evil-test-buffer
      (evil-test-macro "\C-vjjll"
        ";;" " then enter")
      (evil-test-macro "O"
        'bolp ";; then enter")
      (evil-test-macro "o"
        ";;" " This buffer")
      (evil-test-macro "O"
        'bobp ";; This buffer")))
  (ert-info ("Delete a 1x4 block")
    (evil-test-buffer-edit "w\C-ved"
      ";; " " buffer" 'bobp))
  (ert-info ("Delete a 1x2 block")
    (evil-test-buffer-edit "w\C-vjeVo\C-vokd"
      ";; " "is buffer" 'bobp)))

(ert-deftest evil-test-visual-restore ()
  "Test restoring a previous selection"
  :tags '(evil)
  (ert-info ("Start a characterwise selection
if no previous selection")
    (evil-test-buffer-edit ("wgved")
      ";; " " buffer" 'bobp))           ;
  (ert-info ("Restore characterwise selection")
    (evil-test-buffer-edit ("wve" [escape] "gvd")
      ";; " " buffer" 'bobp)
    (evil-test-buffer-edit ("wvjeVov" [escape] "gvd")
      ";; " " you" 'bobp))
  (ert-info ("Restore linewise selection")
    (evil-test-buffer-edit ("wVe" [escape] "gvd")
      'bobp ";; If you want to create a file")
    (evil-test-buffer-edit ("wVjevoV" [escape] "gvd")
      'bobp ";; then enter the text in that file"))
  (ert-info ("Restore blockwise selection")
    (evil-test-buffer-edit ("w\C-ve" [escape] "gvd")
      ";; " " buffer" 'bobp)))

;;; Utilities

(ert-deftest evil-test-concat-lists ()
  "Test `evil-concat-lists' and `evil-concat-alists'"
  :tags '(evil)
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
                   '((a . b))))
    (should-not (equal (evil-concat-lists
                        '((a . b)) '((a . c)))
                       '((a . b))))))

(ert-deftest evil-test-extract-count ()
  "Test `evil-extract-count'"
  :tags '(evil)
  (evil-test-buffer
    (ert-info ("Exact without count")
      (should (equal (evil-extract-count "x")
                     (list nil 'delete-char "x" nil)))
      (should (equal (evil-extract-count "g0")
                     (list nil 'evil-beginning-of-visual-line "g0" nil))))

    (ert-info ("Exact with count")
      (should (equal (evil-extract-count "420x")
                     (list 420 'delete-char "x" nil)))
      (should (equal (evil-extract-count "2301g0")
                     (list 2301 'evil-beginning-of-visual-line "g0" nil))))

    (ert-info ("Extra elements without count")
      (should (equal (evil-extract-count "xAB")
                     (list nil 'delete-char "x" "AB")))
      (should (equal (evil-extract-count "g0CD")
                     (list nil 'evil-beginning-of-visual-line "g0" "CD"))))

    (ert-info ("Extra elements with count")
      (should (equal (evil-extract-count "420xAB")
                     (list 420 'delete-char "x" "AB")))
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

(when evil-tests-run
  (evil-tests-run))

(provide 'evil-tests)

;;; evil-tests.el ends here
