;;; evil-ex.el --- Ex mode  -*- lexical-binding: t; -*-

;; Author: Frank Fischer <frank fischer at mathematik.tu-chemnitz.de>
;; Maintainer: Vegard Øye <vegard_oye at hotmail.com>

;; Version: 1.15.0

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

;; Ex is implemented as an extensible minilanguage, whose grammar
;; is stored in `evil-ex-grammar'.  Ex commands are defined with
;; `evil-ex-define-cmd', which creates a binding from a string
;; to an interactive function.  It is also possible to define key
;; sequences which execute a command immediately when entered:
;; such shortcuts go in `evil-ex-map'.
;;
;; To provide buffer and filename completion, as well as interactive
;; feedback, Ex defines the concept of an argument handler, specified
;; with `evil-ex-define-argument-type'.  In the case of the
;; substitution command (":s/foo/bar"), the handler incrementally
;; highlights matches in the buffer as the substitution is typed.

;;; Code:

(require 'evil-common)
(require 'evil-states)
(require 'evil-types)

(declare-function evil-goto-line "evil-commands")

(eval-when-compile
  (defconst evil-ex-grammar
    '((expression
       (count command argument #'evil-ex-call-command)
       ((\? range) command argument #'evil-ex-call-command)
       (line #'evil-goto-line)
       (sexp #'eval-expression))
      (count
       number)
      (command #'evil-ex-parse-command)
      (binding
       "[~&*@<>=:#]+\\|[[:alpha:]_]+\\|!")
      (emacs-binding
       "[[:alpha:]-][[:alnum:][:punct:]]*")
      (argument
       ((\? "\\(?:.\\|\n\\)+") #'$1))
      (range
       ("%" #'(evil-ex-full-range))
       ("*" #'(evil-ex-last-visual-range))
       ((\? line) "[,;]" (\? line)
        #'(let ((l1 $1))
            (save-excursion
              (and l1 (string= $2 ";") (goto-line l1))
              (evil-ex-range (or l1 (evil-ex-current-line)) $3))))
       (line #'evil-ex-range)
       ("`" marker-name ",`" marker-name
        #'(evil-ex-char-marker-range $2 $4)))
      (line
       (base (\? offset) search (\? offset)
             #'(let ((tmp (evil-ex-line $1 $2)))
                 (save-excursion
                   (goto-line tmp)
                   (evil-ex-line $3 $4))))
       ((\? base) offset search (\? offset)
        #'(let ((tmp (evil-ex-line $1 $2)))
            (save-excursion
              (goto-line tmp)
              (evil-ex-line $3 $4))))
       (base (\? offset) #'evil-ex-line)
       ((\? base) offset #'evil-ex-line))
      (base
       number
       marker
       search
       ("\\^" #'(evil-ex-first-line))
       ("\\$" #'(evil-ex-last-line))
       ("\\." #'(evil-ex-current-line)))
      (offset
       (+ signed-number #'+))
      (marker
       ("'" marker-name #'(evil-ex-marker $2)))
      ;; TODO - handle offset & ;next-pattern search elements
      (search
       forward
       backward
       next
       prev
       subst)
      (forward
       ("/" "\\(?:[\\].\\|[^/]\\)+" "/\\|$" #'(evil-ex-re-fwd $2)))
      (backward
       ("\\?" "\\(?:[\\].\\|[^?]\\)+" "\\?\\|$" #'(evil-ex-re-bwd $2)))
      (marker-name
       "[]\\[-a-zA-Z_<>'}{)(]")
      (next
       "\\\\/" #'(evil-ex-prev-search))
      (prev
       "\\\\\\?" #'(evil-ex-prev-search))
      (subst
       "\\\\&" #'(evil-ex-prev-search))
      (signed-number
       (sign (\? number) #'evil-ex-signed-number))
      (sign
       "\\+\\|-" #'intern)
      (number
       "[0-9]+" #'string-to-number)
      (sexp
       "(.*)" #'(car-safe (read-from-string $0))))
    "Grammar for Ex.
An association list of syntactic symbols and their definitions.
The first entry is the start symbol.  A symbol's definition may
reference other symbols, but the grammar cannot contain
left recursion.  See `evil-parser' for a detailed explanation
of the syntax.")

  (defun evil-parser--dexp (obj)
    "Parse a numerical dollar-sign symbol.
Given e.g. $4, return 4."
    (when (symbolp obj)
      (let ((str (symbol-name obj)))
        (when (string-match "\\$\\([0-9]+\\)" str)
          (string-to-number (match-string 1 str))))))

  (defmacro evil-parser (grammar &rest entrypoints)
    "Construct a parser for GRAMMAR with ENTRYPOINTS.
The result is a function taking the arguments STRING, SYMBOL and
SYNTAX, that parses STRING. SYMBOL should be one of ENTRYPOINTS.

If the parse succeeds, the return value is a cons cell
\(RESULT . END), where RESULT is a parse tree and END is the start of
the remainder of STRING. Otherwise, the return value is nil.

GRAMMAR is an association list of symbols and their definitions.
A definition is a list of production rules, which are tried in
succession.

A production rule can be one of the following:

    nil matches the empty string.
    A regular expression matches a substring.
    A symbol matches a production for that symbol.
    (X Y) matches X followed by Y.
    (\\? X) matches zero or one of X.
    (* X) matches zero or more of X.
    (+ X) matches one or more of X.
    (& X) matches X, but does not consume.
    (! X) matches anything but X, but does not consume.

Thus, a simple grammar may look like:

    ((plus \"\\\\+\")       ; plus <- \"+\"
     (minus \"-\")          ; minus <- \"-\"
     (operator plus minus)) ; operator <- plus / minus

All input-consuming rules have a value. A regular expression evaluates
to the text matched, while a list evaluates to a list of values.
The value of a list may be overridden with a semantic action, which is
specified with a #'-quoted expression at the end:

    (X Y #'foo)

The value of this rule is the result of calling foo with the values
of X and Y as arguments. Alternatively, the function call may be
specified explicitly:

    (X Y #'(foo $1 $2))

Here, $1 refers to X and $2 refers to Y. $0 refers to the whole list.
Dollar expressions can also be used directly:

    (X Y #'$1)

This matches X followed by Y, but ignores the value of Y;
the value of the list is the same as the value of X.

If the SYNTAX argument is non-nil, then all semantic actions
are ignored, and a syntax tree is constructed instead. The
syntax tree obeys the property that all the leaf nodes are
parts of the input string. Thus, by traversing the syntax tree,
one can determine how each character was parsed.

The following symbols have reserved meanings within a grammar:
`\\?', `*', `+', `&', `!', `function', `alt', `seq' and nil."
    (cl-labels
        ;; Return code for parsing PRODUCTION.
        ;; Assumes the variable POS stores the current offset into
        ;; STRING.
        ((compile
          (production)
          (pcase production
            ((or 'nil "") '(cons (when syntax "") pos)) ; Epsilon
            ((and (pred stringp) regexp) ; Token
             `(when
                  ;; Ignore leading whitespace
                  (let ((start (string-match-p "[^ \f\t\n\r\v]\\|\\'" string pos)))
                    (equal (string-match ,regexp string start) start))
                (cons (if syntax (substring string pos (match-end 0))
                        (match-string 0 string))
                      (match-end 0))))
            ((and (pred symbolp) symbol) ; Symbol
             `(let ((pair (,symbol string pos syntax)))
                (and syntax pair
                     (setcar
                      pair
                      (let ((result (car pair)))
                        (cons ',symbol
                              (if (listp result) result (list result))))))
                pair))
            (`(function ,fun) ; Function
             `(let ((pair (funcall #',fun string pos)))
                (and pair syntax (setcar pair (substring string pos (cdr pair))))
                pair))
            ;; Positive lookahead
            (`(& . ,rule) `(when ,(compile rule) ,(compile nil)))
            ;; Negative lookahead
            (`(! . ,rule) `(unless ,(compile rule) ,(compile nil)))
            ;; Zero or one
            (`(\? . ,(or `(,rule) rule)) (compile `(alt ,rule nil)))
            ;; Zero or more
            (`(* . ,rules) (compile `(alt (+ ,@rules) nil)))
            ;; Lists
            ((or `(,(and (or '+ 'alt 'seq) symbol) . ,rules)
                 (and (pred listp) rules (let symbol 'seq)))
             (let ((func (unless (eq symbol 'alt) #'list)))
               (pcase (when (> (length rules) 1) (car (last rules)))
                 (`(function ,x) (setq func x
                                       rules (butlast (copy-sequence rules)))))
               `(let ((pair
                       ,(pcase symbol
                          ('+ ; One or more
                           (when (cdr rules) (error "Too many `+' rules"))
                           `(let ((pos pos) result)
                              (while (let ((x ,(compile (car rules))))
                                       (when x
                                         (push (car x) result)
                                         (< (setq pos (cdr x)) (length string)))))
                              (when result (cons (nreverse result) pos))))
                          ('alt `(or ,@(mapcar #'compile rules)))
                          ('seq
                           (cl-loop
                            for rule in rules collect
                            `(let ((x ,(compile rule)))
                               (when x
                                 (setq pos (cdr x))
                                 ,(if (memq (car-safe rule) '(& !)) t
                                    `(push (car x) result))))
                            into items finally return
                            `(let ((pos pos) result)
                               (and ,@items (cons (nreverse result) pos))))))))
                  ;; Semantic action
                  ,(when func
                     `(when (and pair (not syntax))
                        (let ((result (car pair)))
                          (ignore result) ; Suppress unused var warning
                          (setcar
                           pair
                           ,(pcase func
                              ;; Dollar expression
                              ((or (pred evil-parser--dexp) (pred listp))
                               (dval func))
                              ((pred symbolp)
                               `(,(if (eq symbol 'alt) 'list 'cons) #',func result))
                              (_ (error "Invalid semantic action `%S'" func)))))))
                  pair)))))
         ;; Substitute all dollar-sign symbols in X.
         ;; Each dollar-sign symbol is replaced with the corresponding
         ;; element in RESULT, so that $1 becomes the first element, etc.
         ;; The special value $0 is substituted with the whole list RESULT.
         (dval
          (x)
          (if (listp x) (cons #'list (mapcar #'dval x))
            (let ((num (evil-parser--dexp x)))
              (cond ((null num) `(quote ,x))
                    ((eq num 0) 'result)
                    (t `(nth (1- ,num) result)))))))
      `(lambda (string symbol &optional syntax)
         (cl-labels
             (,@(cl-loop
                 for (symbol . def) in (eval grammar t) collect
                 `(,symbol (string pos syntax) ,(compile `(alt . ,def))))
              (evil-ex-parse-command
               (string pos)
               (let ((result (binding string pos nil)) command end)
                 (when result
                   (setq command (car result)
                         end (cdr result))
                   (cond
                    ;; check whether the parsed command is followed by a slash,
                    ;; dash or number and either the part before is NOT known to be
                    ;; a binding, or the complete string IS known to be a binding
                    ((and (< end (length string))
                          (let ((ch (aref string end)))
                            (or (memq ch '(?- ?/)) (<= ?0 ch ?9)))
                          (or (evil-ex-binding
                               (concat command (substring string end)) t)
                              (not (evil-ex-binding command t))))
                     (emacs-binding string pos nil))
                    ;; parse a following "!" as bang only if the
                    ;; command has the property :ex-bang t
                    ((and (evil-ex-command-force-p command)
                          (< end (length string))
                          (eq (aref string end) ?!))
                     (cons (concat command "!") (1+ end)))
                    (t result))))))
           (pcase symbol
             ,@(cl-loop
                for sym in entrypoints collect
                `(',sym (let ((pos 0)) ,(compile sym))))
             (_ (error "Unknown entrypoint `%s'" symbol))))))))

(defvar evil-ex-echo-overlay nil
  "Overlay used for displaying info messages during Ex.")

(defun evil-ex-p ()
  "Whether Ex is currently active."
  (when evil-ex-current-buffer t))

(evil-define-command evil-ex (&optional initial-input)
  "Enter an Ex command.
The ex command line is initialized with the value of
INITIAL-INPUT. If the command is called interactively the initial
input depends on the current state. If the current state is
normal state and no count argument is given then the initial
input is empty. If a prefix count is given the initial input is
.,.+count. If the current state is visual state then the initial
input is the visual region '<,'> or `<,`>. If the value of the
global variable `evil-ex-initial-input' is non-nil, its content
is appended to the line."
  :keep-visual t
  :repeat abort
  (interactive
   (let ((s (concat
             (cond
              ((and (evil-visual-state-p)
                    evil-ex-visual-char-range
                    (memq (evil-visual-type) '(inclusive exclusive)))
               "`<,`>")
              ((evil-visual-state-p) "'<,'>")
              (current-prefix-arg
               (let ((arg (prefix-numeric-value current-prefix-arg)))
                 (cond ((< arg 0) (setq arg (1+ arg)))
                       ((> arg 0) (setq arg (1- arg))))
                 (if (= arg 0) "."
                   (format ".,.%+d" arg)))))
             evil-ex-initial-input)))
     (list (when (> (length s) 0) s))))
  (let ((evil-ex-current-buffer (current-buffer))
        (evil-ex-previous-command (unless initial-input
                                    (car evil-ex-history)))
        evil-ex-argument-handler result)
    (minibuffer-with-setup-hook
        (lambda ()
          (evil-ex-setup)
          (when initial-input (evil-ex-update)))
      (setq result
            (read-from-minibuffer
             ":"
             (or initial-input
                 (and evil-ex-previous-command
                      evil-want-empty-ex-last-command
                      (propertize evil-ex-previous-command 'face 'shadow)))
             evil-ex-completion-map
             nil
             'evil-ex-history
             (when evil-want-empty-ex-last-command evil-ex-previous-command)
             t)))
    (evil-ex-execute result)))

(defun evil-ex-execute (result)
  "Execute RESULT as an ex command on `evil-ex-current-buffer'."
  ;; empty input means repeating the previous command
  (when (and (zerop (length result))
             evil-want-empty-ex-last-command)
    (setq result evil-ex-previous-command))
  ;; parse data
  (evil-ex-update nil nil nil result)
  ;; execute command
  (unless (zerop (length result))
    (eval (or evil-ex-expression (user-error "Ex: syntax error")))))

(defun evil-ex-delete-backward-char ()
  "Close the minibuffer if it is empty.
Otherwise behaves like `delete-backward-char'."
  (interactive)
  (call-interactively
   (if (zerop (length (minibuffer-contents-no-properties)))
       #'abort-recursive-edit
     #'delete-backward-char)))

(defun evil-ex-abort ()
  "Cancel Ex state when another buffer is selected."
  (unless (or (minibufferp)
              (memq this-command '(mouse-drag-region choose-completion)))
    (abort-recursive-edit)))

(defun evil-ex-command-window-execute (config result)
  (select-window (active-minibuffer-window) t)
  (set-window-configuration config)
  (delete-minibuffer-contents)
  (insert result)
  (exit-minibuffer))

(defun evil--ex-elisp-p ()
  "Return whether an Elisp expression is being entered on the Ex command line."
  (string-prefix-p "(" (minibuffer-contents-no-properties)))

(defun evil-ex-elisp-completion-at-point ()
  "Complete an `evil-ex' Elisp expression."
  (and (evil--ex-elisp-p)
       (fboundp 'elisp-completion-at-point)
       (elisp-completion-at-point)))

(defun evil-ex-setup ()
  "Initialize Ex minibuffer.
This function registers hooks that are used for the interactive
actions during Ex state."
  (add-hook 'post-command-hook #'evil-ex-abort)
  (add-hook 'after-change-functions #'evil-ex-update nil t)
  (add-hook 'minibuffer-exit-hook #'evil-ex-teardown nil t)
  (when evil-ex-previous-command
    (add-hook 'pre-command-hook #'evil-ex-remove-default nil t))
  (set (make-local-variable 'completion-at-point-functions)
       '(evil-ex-elisp-completion-at-point
         evil-ex-command-completion-at-point
         evil-ex-argument-completion-at-point)))

(defun evil-ex-teardown ()
  "Deinitialize Ex minibuffer.
Clean up everything set up by `evil-ex-setup'."
  (remove-hook 'post-command-hook #'evil-ex-abort)
  (when evil-ex-argument-handler
    (let ((runner (evil-ex-argument-handler-runner
                   evil-ex-argument-handler)))
      (when runner
        (funcall runner 'stop)))))
(put 'evil-ex-teardown 'permanent-local-hook t)

(defun evil-ex-update (&optional beg _end _len string)
  "Update Ex variables when the minibuffer changes.
This function is usually called from `after-change-functions'
hook. If BEG is non-nil (which is the case when called from
`after-change-functions'), then an error description is shown
in case of incomplete or unknown commands."
  (let* ((prompt (minibuffer-prompt-end))
         (string (or string (minibuffer-contents-no-properties)))
         arg bang cmd count expr func handler range type)
    (if (and (eq this-command #'self-insert-command)
             (commandp (setq cmd (lookup-key evil-ex-map string))))
        (progn
          (setq evil-ex-expression `(call-interactively #',cmd))
          (when (minibufferp) (exit-minibuffer)))
      (setq cmd nil)
      ;; store the buffer position of each character
      ;; as the `ex-index' text property
      (dotimes (i (length string))
        (put-text-property i (1+ i) 'ex-index (+ i prompt) string))
      (with-current-buffer evil-ex-current-buffer
        (setq expr (evil-ex-parse string))
        (when (eq (car expr) #'evil-ex-call-command)
          (setq count (eval (nth 1 expr))
                cmd (eval (nth 2 expr))
                arg (eval (nth 3 expr))
                range (cond
                       ((evil-range-p count) count)
                       ((numberp count) (evil-ex-range count count)))
                bang (when (string-match-p ".!$" cmd) t))))
      (setq evil-ex-expression expr
            evil-ex-range range
            evil-ex-cmd cmd
            evil-ex-bang bang
            evil-ex-argument arg)
      ;; test the current command
      (when (and cmd (minibufferp))
        (setq func (evil-ex-completed-binding cmd t))
        (cond
         ;; update argument-handler
         (func
          (when (setq type (evil-get-command-property func :ex-arg))
            (setq handler (cdr-safe
                           (assoc type
                                  evil-ex-argument-types))))
          (unless (eq handler evil-ex-argument-handler)
            (let ((runner (and evil-ex-argument-handler
                               (evil-ex-argument-handler-runner
                                evil-ex-argument-handler))))
              (when runner (funcall runner 'stop)))
            (setq evil-ex-argument-handler handler)
            (let ((runner (and evil-ex-argument-handler
                               (evil-ex-argument-handler-runner
                                evil-ex-argument-handler))))
              (when runner (funcall runner 'start evil-ex-argument))))
          (let ((runner (and evil-ex-argument-handler
                             (evil-ex-argument-handler-runner
                              evil-ex-argument-handler))))
            (when runner (funcall runner 'update evil-ex-argument))))
         ;; show error message only when called from `after-change-functions'
         (beg
          (let ((prefix (try-completion cmd (evil-ex-completion-table))))
            (cond
             ((stringp prefix) (evil-ex-echo "Incomplete command"))
             ((null prefix) (evil-ex-echo "Unknown command"))))))))))

(defun evil-ex-echo (string &rest args)
  "Display a message after the current Ex command."
  (with-selected-window (minibuffer-window)
    (unless (or evil-no-display (zerop (length string)))
      (let ((string (format " [%s]" (apply #'format string args)))
            (ov (or evil-ex-echo-overlay
                    (setq evil-ex-echo-overlay (make-overlay (point-min) (point-max) nil t t))))
            after-change-functions before-change-functions)
        (put-text-property 0 (length string) 'face 'evil-ex-info string)
        ;; The following "trick" causes point to be shown before the
        ;; message instead of behind. It is shamelessly stolen from
        ;; the implementation of `minibuffer-message'.
        (put-text-property 0 1 'cursor t string)
        (move-overlay ov (point-max) (point-max))
        (overlay-put ov 'after-string string)
        (add-hook 'pre-command-hook #'evil--ex-remove-echo-overlay nil t)))))

(defun evil--ex-remove-echo-overlay ()
  "Remove echo overlay from Ex minibuffer."
  (when evil-ex-echo-overlay
    (delete-overlay evil-ex-echo-overlay)
    (setq evil-ex-echo-overlay nil))
  (remove-hook 'pre-command-hook #'evil--ex-remove-echo-overlay t))

(defun evil-ex-completion ()
  "Complete the current Ex command or argument."
  (interactive)
  (let (after-change-functions)
    (evil-ex-update)
    (completion-at-point)
    (remove-list-of-text-properties
     (minibuffer-prompt-end) (point-max) '(face evil))))

(defun evil-ex-command-completion-at-point ()
  (let ((beg (if evil-ex-cmd
                 (get-text-property 0 'ex-index evil-ex-cmd)
               (point)))
        (end (point)))
    (list beg end (evil-ex-completion-table) :exclusive 'no)))

(defun evil-ex-completion-table ()
  (let ((ex-cmds
         (cl-loop
          for (cmd . fun) in evil-ex-commands unless (stringp fun)
          collect cmd
          ;; Append ! to all commands that may take a bang argument
          when (evil-get-command-property fun :ex-bang)
          collect (concat cmd "!")))
        (emacs-cmds
         (lambda (str pred action)
           (completion-table-with-predicate
            obarray #'commandp t str pred action))))
    (when (eq evil-ex-complete-emacs-commands t)
      (setq ex-cmds
            (mapcar (lambda (str) (propertize str 'face 'evil-ex-commands))
                    ex-cmds)))
    (cond
     ((null evil-ex-complete-emacs-commands) ex-cmds)
     ((eq evil-ex-complete-emacs-commands 'in-turn)
      (completion-table-in-turn ex-cmds emacs-cmds))
     (t (evil-completion-table-concat ex-cmds emacs-cmds)))))

(defun evil-completion-table-concat (table1 table2)
  (lambda (string pred action)
    (cond
     ((eq action nil)
      (let (matches)
        (dolist (table (list table1 table2) (try-completion string matches))
          (let ((x (try-completion string table pred)))
            (when x (push (if (eq x 't) string x) matches))))))
     ((eq action t)
      (delete-dups
       (append (all-completions string table1 pred)
               (all-completions string table2 pred))))
     ((eq action 'lambda)
      (when (or (test-completion string table1 pred)
                (test-completion string table2 pred))
        t))
     ((eq (car-safe action) 'boundaries)
      (or (completion-boundaries string table1 pred (cdr action))
          (completion-boundaries string table2 pred (cdr action))))
     ((eq action 'metadata)
      '(metadata (display-sort-function . evil-ex-sort-completions))))))

(defun evil-ex-sort-completions (completions)
  (sort completions
        #'(lambda (str1 str2)
            (let ((p1 (eq 'evil-ex-commands (get-text-property 0 'face str1)))
                  (p2 (eq 'evil-ex-commands (get-text-property 0 'face str2))))
              (if (equal p1 p2)
                  (string< str1 str2)
                p1)))))

(defun evil-ex-command-collection (string predicate action)
  (declare (obsolete evil-ex-completion-table "1.15.0"))
  (let* (evil-ex-complete-emacs-commands
         (commands (evil-ex-completion-table)))
    (cond
     ((eq action nil) (try-completion string commands predicate))
     ((eq action t) (all-completions string commands predicate))
     ((eq action 'lambda) (test-completion string commands))
     ((eq (car-safe action) 'boundaries)
      `(boundaries 0 . ,(length (cdr action)))))))

(defun evil-ex-argument-completion-at-point ()
  (let ((context (evil-ex-syntactic-context)))
    (when (memq 'argument context)
      ;; if it's an autoload, load the function; this allows external
      ;; packages to register autoloaded ex commands which will be
      ;; loaded when ex argument completion is triggered
      (let ((binding-definition (symbol-function (evil-ex-binding evil-ex-cmd))))
        (when (autoloadp binding-definition)
          (autoload-do-load binding-definition)))

      (let* ((beg (or (and evil-ex-argument
                           (get-text-property 0 'ex-index evil-ex-argument))
                      (point)))
             (end (1+ (or (and evil-ex-argument
                               (get-text-property (1- (length evil-ex-argument))
                                                  'ex-index evil-ex-argument))
                          (1- (point)))))
             (binding (evil-ex-completed-binding evil-ex-cmd))
             (arg-type (evil-get-command-property binding :ex-arg))
             (arg-handler (assoc arg-type evil-ex-argument-types))
             (completer (when arg-handler
                          (evil-ex-argument-handler-completer
                           (cdr arg-handler)))))
        (when completer
          (if (eq (car completer) 'collection)
              (list beg end (cdr completer))
            (save-restriction
              (narrow-to-region beg (point-max))
              (funcall (cdr completer)))))))))

(defun evil-ex-define-cmd (cmd function)
  "Bind the function FUNCTION to the command CMD."
  (if (string-match "^[^][]*\\(\\[\\(.*\\)\\]\\)[^][]*$" cmd)
      (let ((abbrev (replace-match "" nil t cmd 1))
            (full (replace-match "\\2" nil nil cmd 1)))
        (evil--add-to-alist 'evil-ex-commands full function)
        (evil--add-to-alist 'evil-ex-commands abbrev full))
    (evil--add-to-alist 'evil-ex-commands cmd function)))

(defun evil-ex-make-argument-handler (runner completer)
  (list runner completer))

(defun evil-ex-argument-handler-runner (arg-handler)
  (car arg-handler))

(defun evil-ex-argument-handler-completer (arg-handler)
  (cadr arg-handler))

(defmacro evil-ex-define-argument-type (arg-type doc &rest body)
  "Define a new handler for argument-type ARG-TYPE.
DOC is the documentation string. It is followed by a list of
keywords and function:

:collection COLLECTION

  A collection for completion as required by `all-completions'.

:completion-at-point FUNC

  Function to be called to initialize a potential
  completion. FUNC must match the requirements as described for
  the variable `completion-at-point-functions'. When FUNC is
  called the minibuffer content is narrowed to exactly match the
  argument.

:runner FUNC

  Function to be called when the type of the current argument
  changes or when the content of this argument changes. This
  function should take one obligatory argument FLAG followed by
  an optional argument ARG. FLAG is one of three symbol 'start,
  'stop or 'update. When the argument type is recognized for the
  first time and this handler is started the FLAG is 'start. If
  the argument type changes to something else or ex state
  finished the handler FLAG is 'stop. If the content of the
  argument has changed FLAG is 'update. If FLAG is either 'start
  or 'update then ARG is the current value of this argument. If
  FLAG is 'stop then arg is nil."
  (declare (indent defun)
           (doc-string 2)
           (debug (&define name
                           [&optional stringp]
                           [&rest [keywordp function-form]])))
  (unless (stringp doc) (push doc body))
  (let (runner completer)
    (while (keywordp (car-safe body))
      (let ((key (pop body))
            (func (pop body)))
        (cond
         ((eq key :runner)
          (setq runner func))
         ((eq key :collection)
          (setq completer (cons 'collection func)))
         ((eq key :completion-at-point)
          (setq completer (cons 'completion-at-point func))))))
    `(eval-and-compile
       (evil--add-to-alist
        'evil-ex-argument-types
        ',arg-type
        '(,runner ,completer)))))

(evil-ex-define-argument-type file
  "Handle a file argument."
  :collection read-file-name-internal)

(evil-ex-define-argument-type buffer
  "Called to complete a buffer name argument."
  :collection internal-complete-buffer)

(declare-function comint-completion-at-point "comint")
(declare-function shell-completion-vars "shell" ())

(defun evil-ex-init-shell-argument-completion (flag &optional _arg)
  "Prepare the current minibuffer for completion of shell commands.
This function must be called from the :runner function of some
argument handler that requires shell completion."
  (when (and (eq flag 'start)
             (not evil-ex-shell-argument-initialized))
    (set (make-local-variable 'evil-ex-shell-argument-initialized) t)
    (require 'shell)
    ;; Set up Comint for Shell mode, except
    ;; `comint-completion-at-point' will be called manually.
    (let (completion-at-point-functions)
      (shell-completion-vars))))

(define-obsolete-function-alias
  'evil-ex-shell-command-completion-at-point
  'comint-completion-at-point "1.2.13")

(evil-ex-define-argument-type shell
  "Shell argument type, supports completion."
  :completion-at-point comint-completion-at-point
  :runner evil-ex-init-shell-argument-completion)

(defun evil-ex-file-or-shell-command-completion-at-point ()
  (if (eq (char-after (point-min)) ?!)
      (save-restriction
        (narrow-to-region (1+ (point-min)) (point-max))
        (comint-completion-at-point))
    (list (point-min) (point-max) #'read-file-name-internal)))

(evil-ex-define-argument-type file-or-shell
  "File or shell argument type.
If the current argument starts with a ! the rest of the argument
is considered a shell command, otherwise a file-name. Completion
works accordingly."
  :completion-at-point evil-ex-file-or-shell-command-completion-at-point
  :runner evil-ex-init-shell-argument-completion)

(defun evil-ex-binding (command &optional noerror)
  "Return the final binding of COMMAND."
  (string-match "^\\(.+?\\)\\!?$" command)
  (let ((binding (match-string 1 command)))
    (while (stringp
            (setq binding (cdr (assoc binding evil-ex-commands)))))
    (unless binding
      (setq binding (intern-soft command)))
    (if (commandp binding)
        (or (command-remapping binding) binding)
      (unless noerror
        (user-error "Unknown command: `%s'" command)))))

(defun evil-ex-completed-binding (command &optional noerror)
  "Return the final binding of the completion of COMMAND."
  (let ((completion (try-completion command evil-ex-commands)))
    (evil-ex-binding (if (eq completion t) command
                       (or completion command))
                     noerror)))

;;; TODO: extensions likes :p :~ <cfile> ...
(defun evil-ex-replace-special-filenames (file-name)
  "Replace special symbols in FILE-NAME.
Replaces % by the current file-name,
Replaces # by the alternate file-name in FILE-NAME."
  (let ((remote (file-remote-p file-name))
        (current-fname (buffer-file-name))
        (alternate-fname (and (other-buffer)
                              (buffer-file-name (other-buffer)))))
    (setq file-name (or (file-remote-p file-name 'localname) file-name))
    (when current-fname
      (setq current-fname (or (file-remote-p current-fname 'localname)
                              current-fname))
      (setq file-name
            (replace-regexp-in-string "\\(^\\|[^\\\\]\\)\\(%\\)"
                                      current-fname file-name
                                      t t 2)))
    (when alternate-fname
      (setq alternate-fname (or (file-remote-p alternate-fname 'localname)
                                alternate-fname))
      (setq file-name
            (replace-regexp-in-string "\\(^\\|[^\\\\]\\)\\(#\\)"
                                      alternate-fname file-name
                                      t t 2)))
    (setq file-name
          (replace-regexp-in-string "\\\\\\([#%]\\)"
                                    "\\1" file-name t))
    (setq file-name (concat remote file-name)))
  file-name)

(defun evil-ex-file-arg ()
  "Return the current Ex argument as a file name.
This function interprets special file names like # and %."
  (unless (zerop (length evil-ex-argument))
    (evil-ex-replace-special-filenames evil-ex-argument)))

(defun evil-ex-repeat (&optional count)
  "Repeat the last Ex command."
  (interactive "P")
  (when count
    (goto-char (point-min))
    (forward-line (1- count)))
  (let ((evil-ex-current-buffer (current-buffer))
        (hist evil-ex-history))
    (while hist
      (let ((evil-ex-last-cmd (pop hist)))
        (when evil-ex-last-cmd
          (evil-ex-update nil nil nil evil-ex-last-cmd)
          (let ((binding (evil-ex-binding evil-ex-cmd)))
            (unless (eq binding #'evil-ex-repeat)
              (setq hist nil)
              (eval (or evil-ex-expression
                        (user-error "Ex: syntax error"))))))))))

(defun evil-ex-call-command (range command argument)
  "Execute the given command COMMAND."
  (let* ((count (when (numberp range) range))
         (range (when (evil-range-p range) range))
         (bang (when (string-match-p ".!$" command) t))
         (evil-ex-point (point))
         (evil-ex-range
          (or range (and count (evil-ex-range count count))))
         (evil-ex-command (evil-ex-completed-binding command))
         (restore-point (when (evil-get-command-property evil-ex-command :restore-point)
                          (if (evil-visual-state-p)
                              (min (point) (or (mark) most-positive-fixnum))
                            (point))))
         (evil-ex-bang bang)
         (evil-ex-argument (copy-sequence argument))
         (evil-this-type (evil-type evil-ex-range))
         (current-prefix-arg count)
         (prefix-arg current-prefix-arg))
    (when (stringp evil-ex-argument)
      (set-text-properties
       0 (length evil-ex-argument) nil evil-ex-argument))
    (when evil-ex-reverse-range
      (setq evil-ex-reverse-range nil)
      (unless (y-or-n-p "Backward range given, OK to swap? ")
        (user-error "")))
    (let ((buf (current-buffer)))
      (unwind-protect
          (cond
           ((not evil-ex-range)
            (setq this-command evil-ex-command)
            (evil-exit-visual-state)
            (run-hooks 'pre-command-hook)
            (call-interactively evil-ex-command)
            (run-hooks 'post-command-hook))
           (t
            ;; set visual selection to match the region if an explicit
            ;; range has been specified
            (cl-destructuring-bind (beg end &rest)
                (evil-expand-range evil-ex-range t)
              (setq this-command evil-ex-command)
              (run-hooks 'pre-command-hook)
              (set-mark end)
              (goto-char beg)
              (activate-mark)
              (call-interactively evil-ex-command)
              (run-hooks 'post-command-hook)
              (when restore-point (goto-char restore-point)))))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (deactivate-mark)))))))

(defun evil-ex-line (base &optional offset)
  "Return the line number of BASE plus OFFSET."
  (+ (or base (line-number-at-pos))
     (or offset 0)))

(defun evil-ex-first-line ()
  "Return the line number of the first line."
  (line-number-at-pos (point-min)))

(defun evil-ex-current-line ()
  "Return the line number of the current line."
  (line-number-at-pos))

(defun evil-ex-last-line ()
  "Return the line number of the last line."
  (save-excursion
    (goto-char (point-max))
    (when (bolp)
      (forward-line -1))
    (line-number-at-pos)))

(defun evil-ex-range (beg-line &optional end-line)
  "Return the first and last position of the current range."
  (when (and end-line (< end-line beg-line))
    (setq evil-ex-reverse-range t)
    (evil-swap beg-line end-line))
  (evil-range
   (evil-line-position beg-line)
   (evil-line-position (or end-line beg-line) -1)
   'line
   :expanded t))

(defun evil-ex-full-range ()
  "Return a range encompassing the whole buffer."
  (evil-range (point-min) (point-max) 'line :expanded t))

(defun evil-ex-last-visual-range ()
  "Return a linewise range of the last visual selection."
  (evil-line-expand evil-visual-mark evil-visual-point))

(defun evil-ex-marker (marker)
  "Return MARKER's line number in the current buffer.
Signal an error if MARKER is in a different buffer."
  (setq marker (evil-get-marker
                (if (stringp marker) (aref marker 0) marker)))
  (if (numberp marker)
      (line-number-at-pos marker)
    (user-error "Ex does not support markers in other files")))

(defun evil-ex-char-marker-range (beg end)
  (setq beg (evil-get-marker (if (stringp beg) (aref beg 0) beg))
        end (evil-get-marker (if (stringp end) (aref end 0) end)))
  (if (and (numberp beg) (numberp end))
      (evil-expand-range
       (evil-range beg end
                   (if (evil-visual-state-p)
                       (evil-visual-type)
                     'inclusive)))
    (user-error "Ex does not support markers in other files")))

(declare-function evil-ex-make-search-pattern "evil-search")

(defun evil-ex-re-fwd (pattern)
  "Search forward for PATTERN.
Return the line number of the match."
  (when evil-ex-search-vim-style-regexp
    (setq pattern (evil-transform-vim-style-regexp pattern)))
  (setq evil-ex-search-pattern (evil-ex-make-search-pattern pattern)
        evil-ex-search-direction 'forward)
  (condition-case err
      (save-excursion
        (set-text-properties 0 (length pattern) nil pattern)
        (evil-move-end-of-line)
        (when (or (re-search-forward pattern nil t)
                  (progn
                    (goto-char (point-min))
                    (re-search-forward pattern nil t)))
          (line-number-at-pos (1- (match-end 0)))))
    (invalid-regexp
     (evil-ex-echo (cadr err))
     nil)))

(defun evil-ex-re-bwd (pattern)
  "Search backward for PATTERN.
Return the line number of the match."
  (when evil-ex-search-vim-style-regexp
    (setq pattern (evil-transform-vim-style-regexp pattern)))
  (setq evil-ex-search-pattern (evil-ex-make-search-pattern pattern)
        evil-ex-search-direction 'backward)
  (condition-case err
      (save-excursion
        (set-text-properties 0 (length pattern) nil pattern)
        (evil-move-beginning-of-line)
        (when (or (re-search-backward pattern nil t)
                  (progn
                    (goto-char (point-max))
                    (re-search-backward pattern nil t)))
          (line-number-at-pos (match-beginning 0))))
    (invalid-regexp
     (evil-ex-echo (cadr err))
     nil)))

(defun evil-ex-prev-search ()
  (error "Previous search not yet implemented"))

(defun evil-ex-signed-number (sign &optional number)
  "Return a signed number like -3 and +1.
NUMBER defaults to 1."
  (funcall sign (or number 1)))

;; function `evil-ex-eval' has been superseded by `evil-ex-parse' plus `eval'
(make-obsolete 'evil-ex-eval 'evil-ex-parse "1.2.14")

(defun evil-ex-parse (string &optional syntax start)
  "Parse STRING as an Ex expression and return an evaluation tree.
If SYNTAX is non-nil, return a syntax tree instead.
START is the start symbol, which defaults to `expression'."
  (let ((result (funcall
                 (evil-parser evil-ex-grammar expression range)
                 string (or start 'expression) syntax)))
    (and result
         ;; Disallow incomplete matches (ignore trailing WS)
         (not (string-match-p "[^ \f\t\n\r\v]" string (cdr result)))
         (car result))))

(defun evil-ex-command-force-p (command)
  "Whether COMMAND accepts the bang argument."
  (let ((binding (evil-ex-completed-binding command t)))
    (when binding
      (evil-get-command-property binding :ex-bang))))

(defun evil-ex-syntactic-context (&optional pos)
  "Return the syntactical context of the character at POS.
POS defaults to the current position of point."
  (setq pos (max (- (or pos (point)) (minibuffer-prompt-end)) 0))
  (let* ((tree (evil-ex-parse (minibuffer-contents-no-properties) t))
         (i 0) j result)
    ;; Iterate over syntax tree leaves (i.e. the strings), and return
    ;; the path to the leaf containing the cursor. Or, if not found,
    ;; e.g. because of trailing whitespace, the last leaf allowed to
    ;; be one past the rightmost non-empty string.
    (cl-labels
        ((traverse
          (tree path)
          (when (symbolp (car tree)) (setq path (cons (pop tree) path)))
          (dolist (child tree)
            (if (not (stringp child))
                (traverse child path)
              (setq i (+ i (length child)))
              (when (cond ((>= i pos) (throw 'done path))
                          ((null result) (setq j i))
                          ((>= i j) (setq j (1+ j))))
                (setq result path))))))
      (catch 'done
        (traverse tree nil)
        result))))

(provide 'evil-ex)

;;; evil-ex.el ends here
