;;; evil-ex.el --- Ex-mode -*- lexical-binding: nil -*-

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

(require 'evil-common)
(require 'evil-states)
(require 'evil-types)
(require 'shell)

;;; Code:

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
     "[~&*@<>=:]+\\|[[:alpha:]_]+\\|!")
    (emacs-binding
     "[[:alpha:]-][[:alnum:][:punct:]-]+")
    (bang
     (\? (! space) "!" #'$1))
    (argument
     ((\? space) (\? "\\(?:.\\|\n\\)+") #'$2))
    (range
     ("%" #'(evil-ex-full-range))
     ("*" #'(evil-ex-last-visual-range))
     ((alt "," ";") line #'(evil-ex-range (evil-ex-current-line) $2))
     (line ";" line #'(let ((tmp1 $1))
                        (save-excursion
                          (goto-line tmp1)
                          (evil-ex-range tmp1 $3))))
     (line "," line #'(evil-ex-range $1 $3))
     (line #'(evil-ex-range $1 nil))
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
    (space
     "[ ]+")
    (sexp
     "(.*)" #'(car-safe (read-from-string $1))))
  "Grammar for Ex.
An association list of syntactic symbols and their definitions.
The first entry is the start symbol.  A symbol's definition may
reference other symbols, but the grammar cannot contain
left recursion.  See `evil-parser' for a detailed explanation
of the syntax.")

(defvar evil-ex-echo-overlay nil
  "Overlay used for displaying info messages during ex.")

(defun evil-ex-p ()
  "Whether Ex is currently active."
  (and evil-ex-current-buffer t))

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
   (list
    (let ((s (concat
              (cond
               ((and (evil-visual-state-p)
                     evil-ex-visual-char-range
                     (memq (evil-visual-type) '(inclusive exclusive)))
                "`<,`>")
               ((evil-visual-state-p)
                "'<,'>")
               (current-prefix-arg
                (let ((arg (prefix-numeric-value current-prefix-arg)))
                  (cond ((< arg 0) (setq arg (1+ arg)))
                        ((> arg 0) (setq arg (1- arg))))
                  (if (= arg 0) "."
                    (format ".,.%+d" arg)))))
              evil-ex-initial-input)))
      (and (> (length s) 0) s))))
  (let ((evil-ex-current-buffer (current-buffer))
        (evil-ex-previous-command (unless initial-input
                                    (car-safe evil-ex-history)))
        evil-ex-argument-handler
        evil-ex-info-string
        result)
    (minibuffer-with-setup-hook
        (if initial-input #'evil-ex-setup-and-update #'evil-ex-setup)
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
             (when evil-want-empty-ex-last-command
               evil-ex-previous-command)
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
    (if evil-ex-expression
        (eval evil-ex-expression)
      (user-error "Ex: syntax error"))))

(defun evil-ex-delete-backward-char ()
  "Close the minibuffer if it is empty.
Otherwise behaves like `delete-backward-char'."
  (interactive)
  (call-interactively
   (if (zerop (length (minibuffer-contents)))
       #'abort-recursive-edit
     #'delete-backward-char)))

(defun evil-ex-abort ()
  "Cancel ex state when another buffer is selected."
  (unless (or (minibufferp)
              (memq this-command '(mouse-drag-region choose-completion)))
    (abort-recursive-edit)))

(defun evil-ex-command-window-execute (config result)
  (select-window (active-minibuffer-window) t)
  (set-window-configuration config)
  (delete-minibuffer-contents)
  (insert result)
  (exit-minibuffer))

(defun evil-ex-elisp-completion-at-point ()
  "Complete an `evil-ex' Elisp expression."
  (when (and (fboundp 'elisp-completion-at-point)
             (string-prefix-p "(" (minibuffer-contents-no-properties)))
    (elisp-completion-at-point)))

(defun evil-ex-setup ()
  "Initialize Ex minibuffer.
This function registers several hooks that are used for the
interactive actions during ex state."
  (add-hook 'post-command-hook #'evil-ex-abort)
  (add-hook 'after-change-functions #'evil-ex-update nil t)
  (add-hook 'minibuffer-exit-hook #'evil-ex-teardown nil t)
  (when evil-ex-previous-command
    (add-hook 'pre-command-hook #'evil-ex-remove-default))
  (remove-hook 'minibuffer-setup-hook #'evil-ex-setup)
  (with-no-warnings
    (make-variable-buffer-local 'completion-at-point-functions))
  (setq completion-at-point-functions
        '(evil-ex-elisp-completion-at-point
          evil-ex-command-completion-at-point
          evil-ex-argument-completion-at-point)))
(put 'evil-ex-setup 'permanent-local-hook t)

(defun evil-ex-setup-and-update ()
  "Initialize Ex minibuffer with `evil-ex-setup', then call `evil-ex-update'."
  (evil-ex-setup)
  (evil-ex-update))

(defun evil-ex-teardown ()
  "Deinitialize Ex minibuffer.
Clean up everything set up by `evil-ex-setup'."
  (remove-hook 'post-command-hook #'evil-ex-abort)
  (remove-hook 'minibuffer-exit-hook #'evil-ex-teardown t)
  (remove-hook 'after-change-functions #'evil-ex-update t)
  (when evil-ex-argument-handler
    (let ((runner (evil-ex-argument-handler-runner
                   evil-ex-argument-handler)))
      (when runner
        (funcall runner 'stop)))))
(put 'evil-ex-teardown 'permanent-local-hook t)

(defun evil-ex-update (&optional beg end len string)
  "Update Ex variables when the minibuffer changes.
This function is usually called from `after-change-functions'
hook. If BEG is non-nil (which is the case when called from
`after-change-functions'), then an error description is shown
in case of incomplete or unknown commands."
  (let* ((prompt (minibuffer-prompt-end))
         (string (or string (buffer-substring prompt (point-max))))
         arg bang cmd count expr func handler range tree type)
    (if (and (eq this-command #'self-insert-command)
             (commandp (setq cmd (lookup-key evil-ex-map string))))
        (progn
          (setq evil-ex-expression `(call-interactively #',cmd))
          (when (minibufferp)
            (exit-minibuffer)))
      (setq cmd nil)
      ;; store the buffer position of each character
      ;; as the `ex-index' text property
      (dotimes (i (length string))
        (add-text-properties
         i (1+ i) (list 'ex-index (+ i prompt)) string))
      (with-current-buffer evil-ex-current-buffer
        (setq tree (evil-ex-parse string t)
              expr (evil-ex-parse string))
        (when (eq (car-safe expr) 'evil-ex-call-command)
          (setq count (eval (nth 1 expr))
                cmd (eval (nth 2 expr))
                arg (eval (nth 3 expr))
                range (cond
                       ((evil-range-p count)
                        count)
                       ((numberp count)
                        (evil-ex-range count count)))
                bang (and (save-match-data (string-match ".!$" cmd)) t))))
      (setq evil-ex-tree tree
            evil-ex-expression expr
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
          (when (setq type (evil-get-command-property
                            func :ex-arg))
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
         (beg
          ;; show error message only when called from `after-change-functions'
          (let ((n (length (all-completions cmd (evil-ex-completion-table)))))
            (cond
             ((> n 1) (evil-ex-echo "Incomplete command"))
             ((= n 0) (evil-ex-echo "Unknown command"))))))))))
(put 'evil-ex-update 'permanent-local-hook t)

(defun evil-ex-echo (string &rest args)
  "Display a message after the current Ex command."
  (with-selected-window (minibuffer-window)
    (with-current-buffer (window-buffer (minibuffer-window))
      (unless (or evil-no-display
                  (zerop (length string)))
        (let ((string (format " [%s]" (apply #'format string args)))
              (ov (or evil-ex-echo-overlay
                      (setq evil-ex-echo-overlay (make-overlay (point-min) (point-max) nil t t))))
              after-change-functions before-change-functions)
          (put-text-property 0 (length string) 'face 'evil-ex-info string)
          ;; The following 'trick' causes point to be shown before the
          ;; message instead behind. It is shamelessly stolen from the
          ;; implementation of `minibuffer-message`.
          (put-text-property 0 1 'cursor t string)
          (move-overlay ov (point-max) (point-max))
          (overlay-put ov 'after-string string)
          (add-hook 'pre-command-hook #'evil--ex-remove-echo-overlay nil t))))))

(defun evil--ex-remove-echo-overlay ()
  "Remove echo overlay from ex minibuffer."
  (when evil-ex-echo-overlay
    (delete-overlay evil-ex-echo-overlay)
    (setq evil-ex-echo-overlay nil))
  (remove-hook 'pre-command-hook 'evil--ex-remove-echo-overlay t))

(defun evil-ex-completion ()
  "Complete the current ex command or argument."
  (interactive)
  (let (after-change-functions)
    (evil-ex-update)
    (completion-at-point)
    (remove-text-properties (minibuffer-prompt-end) (point-max) '(face nil evil))))

(defun evil-ex-command-completion-at-point ()
  (let ((beg (if evil-ex-cmd
                 (get-text-property 0 'ex-index evil-ex-cmd)
               (point)))
        (end (point)))
    (list beg end (evil-ex-completion-table) :exclusive 'no)))

(defun evil-ex-completion-table ()
  (cond
   ((eq evil-ex-complete-emacs-commands nil)
    #'evil-ex-command-collection)
   ((eq evil-ex-complete-emacs-commands 'in-turn)
    (completion-table-in-turn
     #'evil-ex-command-collection
     #'(lambda (str pred flag)
         (completion-table-with-predicate
          obarray #'commandp t str pred flag))))
   (t
    #'(lambda (str pred flag)
        (evil-completion-table-concat
         #'evil-ex-command-collection
         #'(lambda (str pred flag)
             (completion-table-with-predicate
              obarray #'commandp t str pred flag))
         str pred flag)))))

(defun evil-completion-table-concat (table1 table2 string pred flag)
  (cond
   ((eq flag nil)
    (let ((result1 (try-completion string table1 pred))
          (result2 (try-completion string table2 pred)))
      (cond
       ((null result1) result2)
       ((null result2) result1)
       ((and (eq result1 t) (eq result2 t)) t)
       (t result1))))
   ((eq flag t)
    (delete-dups
     (append (all-completions string table1 pred)
             (all-completions string table2 pred))))
   ((eq flag 'lambda)
    (and (or (eq t (test-completion string table1 pred))
             (eq t (test-completion string table2 pred)))
         t))
   ((eq (car-safe flag) 'boundaries)
    (or (completion-boundaries string table1 pred (cdr flag))
        (completion-boundaries string table2 pred (cdr flag))))
   ((eq flag 'metadata)
    '(metadata (display-sort-function . evil-ex-sort-completions)))))

(defun evil-ex-sort-completions (completions)
  (sort completions
        #'(lambda (str1 str2)
            (let ((p1 (eq 'evil-ex-commands (get-text-property 0 'face str1)))
                  (p2 (eq 'evil-ex-commands (get-text-property 0 'face str2))))
              (if (equal p1 p2)
                  (string< str1 str2)
                p1)))))

(defun evil-ex-command-collection (cmd predicate flag)
  "Called to complete a command."
  (let (commands)
    ;; append ! to all commands that may take a bang argument
    (dolist (cmd (mapcar #'car evil-ex-commands))
      (push cmd commands)
      (if (evil-ex-command-force-p cmd)
          (push (concat cmd "!") commands)))
    (when (eq evil-ex-complete-emacs-commands t)
      (setq commands
            (mapcar #'(lambda (str) (propertize str 'face 'evil-ex-commands))
                    commands)))
    (cond
     ((eq flag nil) (try-completion cmd commands predicate))
     ((eq flag t) (all-completions cmd commands predicate))
     ((eq flag 'lambda) (test-completion cmd commands))
     ((eq (car-safe flag) 'boundaries)
      `(boundaries 0 . ,(length (cdr flag)))))))

(defun evil-ex-argument-completion-at-point ()
  (let ((context (evil-ex-syntactic-context (1- (point)))))
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
                                                  'ex-index
                                                  evil-ex-argument))
                          (1- (point)))))
             (binding (evil-ex-completed-binding evil-ex-cmd))
             (arg-type (evil-get-command-property binding :ex-arg))
             (arg-handler (assoc arg-type evil-ex-argument-types))
             (completer (and arg-handler
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
  (save-match-data
    (if (string-match "^[^][]*\\(\\[\\(.*\\)\\]\\)[^][]*$" cmd)
        (let ((abbrev (replace-match "" nil t cmd 1))
              (full (replace-match "\\2" nil nil cmd 1)))
          (evil--add-to-alist 'evil-ex-commands full function)
          (evil--add-to-alist 'evil-ex-commands abbrev full))
      (evil--add-to-alist 'evil-ex-commands cmd function))))

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

(declare-function shell-completion-vars "shell" ())

(defun evil-ex-init-shell-argument-completion (flag &optional arg)
  "Prepare the current minibuffer for completion of shell commands.
This function must be called from the :runner function of some
argument handler that requires shell completion."
  (when (and (eq flag 'start)
             (not evil-ex-shell-argument-initialized))
    (set (make-local-variable 'evil-ex-shell-argument-initialized) t)
    (cond
     ;; Emacs 24
     ((fboundp 'comint-completion-at-point)
      (shell-completion-vars))
     (t
      (set (make-local-variable 'minibuffer-default-add-function)
           'minibuffer-default-add-shell-commands)))
    (setq completion-at-point-functions
          '(evil-ex-command-completion-at-point
            evil-ex-argument-completion-at-point))))

(define-obsolete-function-alias
  'evil-ex-shell-command-completion-at-point
  'comint-completion-at-point "1.2.13")

(evil-ex-define-argument-type shell
  "Shell argument type, supports completion."
  :completion-at-point comint-completion-at-point
  :runner evil-ex-init-shell-argument-completion)

(defun evil-ex-file-or-shell-command-completion-at-point ()
  (if (and (< (point-min) (point-max))
           (= (char-after (point-min)) ?!))
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
  (let ((binding (save-match-data
                   (string-match "^\\(.+?\\)\\!?$" command)
                   (match-string 1 command))))
    (while (progn
             (setq binding (cdr (assoc binding evil-ex-commands)))
             (stringp binding)))
    (unless binding
      (setq binding (intern command)))
    (if (commandp binding)
        ;; check for remaps
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
              (if evil-ex-expression
                  (eval evil-ex-expression)
                (user-error "Ex: syntax error")))))))))

(defun evil-ex-call-command (range command argument)
  "Execute the given command COMMAND."
  (let* ((count (when (numberp range) range))
         (range (when (evil-range-p range) range))
         (bang (and (save-match-data (string-match ".!$" command)) t))
         (evil-ex-point (point))
         (evil-ex-range
          (or range (and count (evil-ex-range count count))))
         (evil-ex-command (evil-ex-completed-binding command))
         (restore-point (when (evil-get-command-property evil-ex-command :restore-point)
                          (if (evil-visual-state-p)
                              (min (point) (or (mark) most-positive-fixnum))
                            (point))))
         (evil-ex-bang (and bang t))
         (evil-ex-argument (copy-sequence argument))
         (evil-this-type (evil-type evil-ex-range))
         (current-prefix-arg count)
         (prefix-arg current-prefix-arg))
    (when (stringp evil-ex-argument)
      (set-text-properties
       0 (length evil-ex-argument) nil evil-ex-argument))
    (let ((buf (current-buffer)))
      (when evil-ex-reverse-range
        (setq evil-ex-reverse-range nil)
        (unless (y-or-n-p "Backward range given, OK to swap? ")
          (user-error "")))
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
            (let ((ex-range (evil-copy-range evil-ex-range))
                  beg end)
              (evil-expand-range ex-range)
              (setq beg (evil-range-beginning ex-range)
                    end (evil-range-end ex-range))
              (evil-sort beg end)
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
  (line-number-at-pos (point)))

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
    (let ((beg-line* beg-line))
      (setq beg-line end-line
            end-line beg-line*)))
  (evil-range
   (evil-line-position beg-line)
   (evil-line-position (or end-line beg-line) -1)
   'line
   :expanded t))

(defun evil-ex-full-range ()
  "Return a range encompassing the whole buffer."
  (evil-range (point-min) (point-max) 'line))

(defun evil-ex-last-visual-range ()
  "Return a linewise range of the last visual selection."
  (evil-line-expand evil-visual-mark evil-visual-point))

(defun evil-ex-marker (marker)
  "Return MARKER's line number in the current buffer.
Signal an error if MARKER is in a different buffer."
  (when (stringp marker)
    (setq marker (aref marker 0)))
  (setq marker (evil-get-marker marker))
  (if (numberp marker)
      (line-number-at-pos marker)
    (user-error "Ex does not support markers in other files")))

(defun evil-ex-char-marker-range (beg end)
  (when (stringp beg) (setq beg (aref beg 0)))
  (when (stringp end) (setq end (aref end 0)))
  (setq beg (evil-get-marker beg)
        end (evil-get-marker end))
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
      (save-match-data
        (save-excursion
          (set-text-properties 0 (length pattern) nil pattern)
          (evil-move-end-of-line)
          (if (re-search-forward pattern nil t)
              (line-number-at-pos (1- (match-end 0)))
            (goto-char (point-min))
            (and (re-search-forward pattern nil t)
                 (line-number-at-pos (1- (match-end 0)))))))
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
      (save-match-data
        (save-excursion
          (set-text-properties 0 (length pattern) nil pattern)
          (evil-move-beginning-of-line)
          (if (re-search-backward pattern nil t)
              (line-number-at-pos (match-beginning 0))
            (goto-char (point-max))
            (and (re-search-backward pattern nil t)
                 (line-number-at-pos (match-beginning 0))))))
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
  (let* ((start (or start (car-safe (car-safe evil-ex-grammar))))
         (match (evil-parser
                 string start evil-ex-grammar t syntax)))
    (car-safe match)))

(defun evil-ex-parse-command (string)
  "Parse STRING as an Ex binding."
  (let ((result (evil-parser string 'binding evil-ex-grammar))
        bang command)
    (when result
      (setq command (car-safe result)
            string (cdr-safe result))
      ;; check whether the parsed command is followed by a slash, dash
      ;; or number and either the part before is NOT known to be a binding,
      ;; or the complete string IS known to be a binding
      (when (and (> (length string) 0)
                 (string-match-p "^[-/[:digit:]]" string)
                 (or (evil-ex-binding (concat command string) t)
                     (not (evil-ex-binding command t))))
        (setq result (evil-parser (concat command string)
                                  'emacs-binding
                                  evil-ex-grammar)
              command (car-safe result)
              string (cdr-safe result)))
      ;; parse a following "!" as bang only if
      ;; the command has the property :ex-bang t
      (when (evil-ex-command-force-p command)
        (setq result (evil-parser string 'bang evil-ex-grammar)
              bang (or (car-safe result) "")
              string (cdr-safe result)
              command (concat command bang)))
      (cons command string))))

(defun evil-ex-command-force-p (command)
  "Whether COMMAND accepts the bang argument."
  (let ((binding (evil-ex-completed-binding command t)))
    (when binding
      (evil-get-command-property binding :ex-bang))))

(defun evil-flatten-syntax-tree (tree)
  "Find all paths from the root of TREE to its leaves.
TREE is a syntax tree, i.e., all its leave nodes are strings.
The `nth' element in the result is the syntactic context
for the corresponding string index (counted from zero)."
  (let* ((result nil)
         (traverse nil)
         (traverse
          #'(lambda (tree path)
              (if (stringp tree)
                  (dotimes (char (length tree))
                    (push path result))
                (let ((path (cons (car tree) path)))
                  (dolist (subtree (cdr tree))
                    (funcall traverse subtree path)))))))
    (funcall traverse tree nil)
    (nreverse result)))

(defun evil-ex-syntactic-context (&optional pos)
  "Return the syntactical context of the character at POS.
POS defaults to the current position of point."
  (let* ((contexts (evil-flatten-syntax-tree evil-ex-tree))
         (length (length contexts))
         (pos (- (or pos (point)) (minibuffer-prompt-end))))
    (when (>= pos length)
      (setq pos (1- length)))
    (when (< pos 0)
      (setq pos 0))
    (when contexts
      (nth pos contexts))))

(defun evil-parser--dexp (obj)
  "Parse a numerical dollar-sign symbol.
Given e.g. $4, return 4."
  (when (symbolp obj)
    (let ((str (symbol-name obj)))
      (save-match-data
        (when (string-match "\\$\\([0-9]+\\)" str)
          (string-to-number (match-string 1 str)))))))

(defun evil-parser--dval (obj result)
  "Substitute all dollar-sign symbols in OBJ.
Each dollar-sign symbol is replaced with the corresponding
element in RESULT, so that $1 becomes the first element, etc.
The special value $0 is substituted with the whole list RESULT.
If RESULT is not a list, all dollar-sign symbols are substituted with
RESULT."
  (if (listp obj)
      (mapcar (lambda (obj) (evil-parser--dval obj result)) obj)
    (let ((num (evil-parser--dexp obj)))
      (if num
          (if (not (listp result))
              result
            (if (eq num 0)
                `(list ,@result)
              (nth (1- num) result)))
        obj))))

(defun evil-parser (string symbol grammar &optional greedy syntax)
  "Parse STRING as a SYMBOL in GRAMMAR.
If GREEDY is non-nil, the whole of STRING must match.
If the parse succeeds, the return value is a cons cell
\(RESULT . TAIL), where RESULT is a parse tree and TAIL is
the remainder of STRING. Otherwise, the return value is nil.

GRAMMAR is an association list of symbols and their definitions.
A definition is either a list of production rules, which are
tried in succession, or a #'-quoted function, which is called
to parse the input.

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

    ((plus \"\\\\+\")           ; plus <- \"+\"
     (minus \"-\")            ; minus <- \"-\"
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
syntax tree obeys the property that all the leave nodes are
parts of the input string. Thus, by traversing the syntax tree,
one can determine how each character was parsed.

The following symbols have reserved meanings within a grammar:
`\\?', `*', `+', `&', `!', `function', `alt', `seq' and nil."
  (let ((string (or string ""))
        func pair result rules tail)
    (cond
     ;; epsilon
     ((member symbol '("" nil))
      (setq pair (cons (if syntax "" nil) string)))
     ;; token
     ((stringp symbol)
      (save-match-data
        (when (or (eq (string-match symbol string) 0)
                  ;; ignore leading whitespace
                  (and (eq (string-match "^[ \f\t\n\r\v]+" string) 0)
                       (eq (match-end 0)
                           (string-match
                            symbol string (match-end 0)))))
          (setq result (match-string 0 string)
                tail (substring string (match-end 0))
                pair (cons result tail))
          (when (and syntax pair)
            (setq result (substring string 0
                                    (- (length string)
                                       (length tail))))
            (setcar pair result)))))
     ;; symbol
     ((symbolp symbol)
      (let ((context symbol))
        (setq rules (cdr-safe (assq symbol grammar)))
        (setq pair (evil-parser string `(alt ,@rules)
                                grammar greedy syntax))
        (when (and syntax pair)
          (setq result (car pair))
          (if (and (listp result) (sequencep (car result)))
              (setq result `(,symbol ,@result))
            (setq result `(,symbol ,result)))
          (setcar pair result))))
     ;; function
     ((eq (car-safe symbol) 'function)
      (setq symbol (cadr symbol)
            pair (funcall symbol string))
      (when (and syntax pair)
        (setq tail (or (cdr pair) "")
              result (substring string 0
                                (- (length string)
                                   (length tail))))
        (setcar pair result)))
     ;; list
     ((listp symbol)
      (setq rules symbol
            symbol (car-safe rules))
      (if (memq symbol '(& ! \? * + alt seq))
          (setq rules (cdr rules))
        (setq symbol 'seq))
      (when (and (memq symbol '(+ alt seq))
                 (> (length rules) 1))
        (setq func (car (last rules)))
        (if (eq (car-safe func) 'function)
            (setq rules (delq func (copy-sequence rules))
                  func (cadr func))
          (setq func nil)))
      (cond
       ;; positive lookahead
       ((eq symbol '&)
        (when (evil-parser string rules grammar greedy syntax)
          (setq pair (evil-parser string nil grammar nil syntax))))
       ;; negative lookahead
       ((eq symbol '!)
        (unless (evil-parser string rules grammar greedy syntax)
          (setq pair (evil-parser string nil grammar nil syntax))))
       ;; zero or one
       ((eq symbol '\?)
        (setq rules (if (> (length rules) 1)
                        `(alt ,rules nil)
                      `(alt ,@rules nil))
              pair (evil-parser string rules grammar greedy syntax)))
       ;; zero or more
       ((eq symbol '*)
        (setq rules `(alt (+ ,@rules) nil)
              pair (evil-parser string rules grammar greedy syntax)))
       ;; one or more
       ((eq symbol '+)
        (let (current results)
          (catch 'done
            (while (setq current (evil-parser
                                  string rules grammar nil syntax))
              (setq result (car-safe current)
                    tail (or (cdr-safe current) "")
                    results (append results (if syntax result
                                              (cdr-safe result))))
              ;; stop if stuck
              (if (equal string tail)
                  (throw 'done nil)
                (setq string tail))))
          (when results
            (setq func (or func 'list)
                  pair (cons results tail)))))
       ;; alternatives
       ((eq symbol 'alt)
        (catch 'done
          (dolist (rule rules)
            (when (setq pair (evil-parser
                              string rule grammar greedy syntax))
              (throw 'done pair)))))
       ;; sequence
       (t
        (setq func (or func 'list))
        (let ((last (car-safe (last rules)))
              current results rule)
          (catch 'done
            (while rules
              (setq rule (pop rules)
                    current (evil-parser string rule grammar
                                         (when greedy
                                           (null rules))
                                         syntax))
              (cond
               ((null current)
                (setq results nil)
                (throw 'done nil))
               (t
                (setq result (car-safe current)
                      tail (cdr-safe current))
                (unless (memq (car-safe rule) '(& !))
                  (if (and syntax
                           (or (null result)
                               (and (listp result)
                                    (listp rule)
                                    ;; splice in single-element
                                    ;; (\? ...) expressions
                                    (not (and (eq (car-safe rule) '\?)
                                              (eq (length rule) 2))))))
                      (setq results (append results result))
                    (setq results (append results (list result)))))
                (setq string (or tail ""))))))
          (when results
            (setq pair (cons results tail))))))
      ;; semantic action
      (when (and pair func (not syntax))
        (setq result (car pair))
        (cond
         ((null func)
          (setq result nil))
         ;; lambda function
         ((eq (car-safe func) 'lambda)
          (if (memq symbol '(+ seq))
              (setq result `(funcall ,func ,@result))
            (setq result `(funcall ,func ,result))))
         ;; string replacement
         ((or (stringp func) (stringp (car-safe func)))
          (let* ((symbol (or (car-safe (cdr-safe func))
                             (and (boundp 'context) context)
                             (car-safe (car-safe grammar))))
                 (string (if (stringp func) func (car-safe func))))
            (setq result (car-safe (evil-parser string symbol grammar
                                                greedy syntax)))))
         ;; dollar expression
         ((evil-parser--dexp func)
          (setq result (evil-parser--dval func result)))
         ;; function call
         ((listp func)
          (setq result (evil-parser--dval func result)))
         ;; symbol
         (t
          (if (memq symbol '(+ seq))
              (setq result `(,func ,@result))
            (setq result `(,func ,result)))))
        (setcar pair result))))
    ;; weed out incomplete matches
    (when pair
      (if (not greedy) pair
        (if (null (cdr pair)) pair
          ;; ignore trailing whitespace
          (when (save-match-data (string-match "^[ \f\t\n\r\v]*$" (cdr pair)))
            (unless syntax (setcdr pair nil))
            pair))))))

(provide 'evil-ex)

;;; evil-ex.el ends here
