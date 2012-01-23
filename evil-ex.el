;;; Ex-mode

;; Ex is implemented as an extensible minilanguage, whose grammar
;; is stored in `evil-ex-grammar'. Ex commands are defined with
;; `evil-ex-define-cmd', which creates a binding from a string
;; to an interactive function. It is also possible to define key
;; sequences which execute a command immediately when entered:
;; such shortcuts go in `evil-ex-map'.
;;
;; To provide buffer and filename completion, as well as interactive
;; feedback, Ex defines the concept of an argument handler, specified
;; with `evil-ex-define-argument-type'. In the case of the
;; substitution command (":s/foo/bar"), the handler incrementally
;; highlights matches in the buffer as the substitution is typed.

(require 'evil-common)
(require 'evil-states)

(defconst evil-ex-grammar
  '((expression
     (count command (\? argument) #'evil-ex-call-command)
     ((\? range) command (\? argument) #'evil-ex-call-command)
     (line #'evil-goto-line)
     (sexp #'eval-expression))
    (count
     number)
    (command #'evil-ex-parse-command)
    (binding
     "[*@<>=:a-zA-Z_-]+\\|!")
    (force
     (\? (! space) "!" #'$1))
    (argument
     ((\? space) (\? ".+") #'$2))
    (range
     (address (\? "[,;]" address #'$2) #'evil-ex-range)
     ("%" #'(evil-ex-full-range)))
    (address
     (line (\? offset) #'evil-ex-address)
     ((\? line) offset #'evil-ex-address))
    (line
     number
     marker
     search
     ("\\^" #'(evil-ex-first-line))
     ("\\$" #'(evil-ex-last-line))
     ("\\." #'(evil-ex-current-line)))
    (offset
     (+ signed-number #'+))
    (marker
     ("'" "[-a-zA-Z_<>']" #'(evil-ex-marker $2)))
    (search
     forward
     backward
     next
     prev
     subst)
    (forward
     ("/" "\\(?:[\\].\\|[^/,; ]\\)+" (! "/")
      #'(evil-ex-re-fwd $2))
     ("/" "\\(?:[\\].\\|[^/]\\)+" "/"
      #'(evil-ex-re-fwd $2)))
    (backward
     ("\\?" "\\(?:[\\].\\|[^?,; ]\\)+" (! "\\?")
      #'(evil-ex-re-bwd $2))
     ("\\?" "\\(?:[\\].\\|[^?]\\)+" "\\?"
      #'(evil-ex-re-bwd $2)))
    (next
     "/" #'(evil-ex-prev-search))
    (prev
     "\\?" #'(evil-ex-prev-search))
    (subst
     "&" #'(evil-ex-prev-search))
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
The first entry is the start symbol. A symbol's definition may
reference other symbols, but the grammar cannot contain
left recursion. See `evil-parser' for a detailed explanation
of the syntax.")

(defun evil-ex-p ()
  "Whether Ex is currently active."
  (and evil-ex-current-buffer t))

(evil-define-command evil-ex (&optional initial-input)
  "Enter an Ex command."
  :keep-visual t
  (interactive
   (cond
    ((evil-visual-state-p)
     '("'<,'>"))
    (current-prefix-arg
     (let ((arg (prefix-numeric-value current-prefix-arg)))
       (cond ((< arg 0) (setq arg (1+ arg)))
             ((> arg 0) (setq arg (1- arg))))
       (if (= arg 0) '(".")
         `(,(format ".,.%+d" arg)))))))
  (let ((minibuffer-local-completion-map evil-ex-completion-map)
        (evil-ex-current-buffer (current-buffer))
        (evil-ex-previous-command (unless initial-input
                                    (car-safe evil-ex-history)))
        evil-ex-argument-handler evil-ex-info-string result)
    (add-hook 'minibuffer-setup-hook #'evil-ex-setup)
    (setq result
          (completing-read ":" #'evil-ex-completion nil nil
                           (or initial-input
                               (and evil-ex-previous-command
                                    (format "(default: %s) "
                                            evil-ex-previous-command)))
                           'evil-ex-history evil-ex-previous-command t))
    (evil-ex-update nil nil nil result)
    (unless (zerop (length result))
      (if evil-ex-expression
          (eval evil-ex-expression)
        (error "Ex: syntax error")))))

(defun evil-ex-delete-backward-char ()
  "Close the minibuffer if it is empty.
Otherwise behaves like `delete-backward-char'."
  (interactive)
  (call-interactively
   (if (zerop (length (minibuffer-contents)))
       #'abort-recursive-edit
     #'delete-backward-char)))

(defun evil-ex-setup ()
  "Initialize Ex minibuffer."
  (add-hook 'after-change-functions #'evil-ex-update nil t)
  (add-hook 'minibuffer-exit-hook #'evil-ex-teardown)
  (when evil-ex-previous-command
    (add-hook 'pre-command-hook #'evil-ex-remove-default))
  (remove-hook 'minibuffer-setup-hook #'evil-ex-setup))
(put 'evil-ex-setup 'permanent-local-hook t)

(defun evil-ex-teardown ()
  "Deinitialize Ex minibuffer."
  (remove-hook 'minibuffer-exit-hook #'evil-ex-teardown)
  (remove-hook 'after-change-functions #'evil-ex-update t)
  (when evil-ex-argument-handler
    (funcall evil-ex-argument-handler 'stop)))
(put 'evil-ex-teardown 'permanent-local-hook t)

(defun evil-ex-remove-default ()
  (delete-minibuffer-contents)
  (remove-hook 'pre-command-hook #'evil-ex-remove-default))
(put 'evil-ex-remove-default 'permanent-local-hook t)

(defun evil-ex-update (&optional beg end len string)
  "Update Ex variables when the minibuffer changes."
  (let* ((prompt (minibuffer-prompt-end))
         (string (or string (buffer-substring prompt (point-max))))
         arg arg-handler arg-type cmd count expr force func range tree)
    (cond
     ((commandp (setq cmd (lookup-key evil-ex-map string)))
      (setq evil-ex-expression `(call-interactively #',cmd))
      (when (minibufferp)
        (exit-minibuffer)))
     (t
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
                force (and (string-match ".!$" cmd) t))))
      (setq evil-ex-tree tree
            evil-ex-expression expr
            evil-ex-range range
            evil-ex-command cmd
            evil-ex-force force
            evil-ex-argument arg)
      ;; test the current command
      (when (and cmd (minibufferp))
        (setq func (evil-ex-completed-binding cmd t))
        (cond
         ;; update arg-handler
         (func
          (when (setq arg-type (evil-get-command-property
                                func :ex-arg))
            (setq arg-handler (cdr-safe
                               (assoc arg-type
                                      evil-ex-argument-types))))
          (unless (eq arg-handler evil-ex-argument-handler)
            (when evil-ex-argument-handler
              (funcall evil-ex-argument-handler 'stop))
            (setq evil-ex-argument-handler arg-handler)
            (when evil-ex-argument-handler
              (funcall evil-ex-argument-handler
                       'start evil-ex-argument)))
          (when evil-ex-argument-handler
            (funcall evil-ex-argument-handler
                     'update evil-ex-argument)))
         ((all-completions cmd evil-ex-commands)
          (evil-ex-echo "Incomplete command"))
         (t
          (evil-ex-echo "Unknown command"))))))))
(put 'evil-ex-update 'permanent-local-hook t)

(defun evil-ex-echo (string &optional args)
  "Display a message after the current Ex command."
  (with-selected-window (minibuffer-window)
    (with-current-buffer (window-buffer (minibuffer-window))
      (unless (or evil-no-display
                  (zerop (length string)))
        (let ((string (format " [%s]" (apply #'format string args)))
              after-change-functions before-change-functions)
          (put-text-property 0 (length string) 'face 'evil-ex-info string)
          (minibuffer-message string))))))

(defun evil-ex-define-cmd (cmd function)
  "Binds the function FUNCTION to the command CMD."
  (if (string-match "^[^][]*\\(\\[\\(.*\\)\\]\\)[^][]*$" cmd)
      (let ((abbrev (replace-match "" nil t cmd 1))
            (full (replace-match "\\2" nil nil cmd 1)))
        (evil-add-to-alist 'evil-ex-commands full function)
        (evil-add-to-alist 'evil-ex-commands abbrev full))
    (evil-add-to-alist 'evil-ex-commands cmd function)))

(defmacro evil-ex-define-argument-type (arg-type args &rest body)
  "Defines a new handler for argument-type ARG-TYPE."
  (declare (indent defun)
           (debug (&define symbolp lambda-list
                           [&optional ("interactive" [&rest form])]
                           def-body)))
  (let ((name (intern (format "evil-ex-argument-handler-%s"
                              arg-type))))
    `(progn
       (defun ,name ,args ,@body)
       (evil-add-to-alist 'evil-ex-argument-types ',arg-type ',name)
       ',arg-type)))

(evil-ex-define-argument-type file (flag &rest args)
  "Handles a file argument."
  (when (eq flag 'complete)
    (let ((arg (pop args))
          (predicate (pop args))
          (flag (pop args)))
      (if (null arg)
          default-directory
        (let ((dir (or (file-name-directory arg)
                       (with-current-buffer evil-ex-current-buffer
                         default-directory)))
              (fname (file-name-nondirectory arg)))
          (cond
           ((null dir) (ding))
           ((null flag)
            (let ((result (file-name-completion fname dir)))
              (cond
               ((null result) nil)
               ((eq result t) t)
               (t (concat dir result)))))

           ((eq t flag)
            (file-name-all-completions fname dir))

           ((eq 'lambda flag)
            (eq (file-name-completion fname dir) t))))))))

(evil-ex-define-argument-type buffer (flag &rest args)
  "Called to complete a buffer name argument."
  (when (eq flag 'complete)
    (let ((arg (pop args))
          (predicate (pop args))
          (flag (pop args)))
      (when arg
        (let ((buffers (mapcar #'(lambda (buffer)
                                   (cons (buffer-name buffer) nil))
                               (buffer-list t))))
          (cond
           ((null flag)
            (try-completion arg buffers predicate))
           ((eq t flag)
            (all-completions arg buffers predicate))
           ((eq 'lambda flag)
            (test-completion arg buffers predicate))))))))

(defun evil-ex-binding (command &optional noerror)
  "Returns the final binding of COMMAND."
  (let ((binding command))
    (when binding
      (string-match "^\\(.+?\\)\\!?$" binding)
      (setq binding (match-string 1 binding))
      (while (progn
               (setq binding (cdr (assoc binding evil-ex-commands)))
               (stringp binding)))
      (unless binding
        (setq binding (intern command)))
      (if (commandp binding)
          binding
        (unless noerror
          (error "Unknown command: `%s'" command))))))

(defun evil-ex-completed-binding (command &optional noerror)
  "Returns the final binding of the completion of COMMAND."
  (let ((completion (try-completion command evil-ex-commands)))
    (evil-ex-binding (if (eq completion t) command
                       (or completion command))
                     noerror)))

;;; TODO: extensions likes :p :~ <cfile> ...
(defun evil-ex-replace-special-filenames (file-name)
  "Replace special symbols in FILE-NAME.
Replaces % by the current file-name,
Replaces # by the alternate file-name in FILE-NAME."
  (let ((current-fname (buffer-file-name))
        (alternate-fname (and (other-buffer)
                              (buffer-file-name (other-buffer)))))
    (when current-fname
      (setq file-name
            (replace-regexp-in-string "\\(^\\|[^\\\\]\\)\\(%\\)"
                                      current-fname file-name
                                      t t 2)))
    (when alternate-fname
      (setq file-name
            (replace-regexp-in-string "\\(^\\|[^\\\\]\\)\\(#\\)"
                                      alternate-fname file-name
                                      t t 2)))
    (setq file-name
          (replace-regexp-in-string "\\\\\\([#%]\\)"
                                    "\\1" file-name t)))
  file-name)

(defun evil-ex-file-arg ()
  "Returns the current Ex argument as a file name.
This function interprets special file names like # and %."
  (unless (or (null evil-ex-argument)
              (zerop (length evil-ex-argument)))
    (evil-ex-replace-special-filenames evil-ex-argument)))

(defun evil-ex-complete ()
  "Start Ex minibuffer completion.
Temporarily disables update functions."
  (interactive)
  (let (after-change-functions before-change-functions)
    (minibuffer-complete)))

;; TODO: Emacs 22 completion boundaries
(defun evil-ex-completion (string predicate flag)
  "Complete an object in the Ex buffer."
  (let* ((boundaries (cdr-safe flag))
         (prompt (minibuffer-prompt-end))
         context prefix result start)
    (when (= (point) (+ (length string) prompt))
      (evil-ex-update)
      (setq context (evil-ex-syntactic-context (1- (point))))
      (cond
       ;; complete command
       ((memq 'command context)
        (setq start (or (get-text-property
                         0 'ex-index evil-ex-command) (point))
              prefix (buffer-substring prompt start))
        (if boundaries
            (setq boundaries (cons 'boundaries
                                   (cons (- start prompt) 0)))
          (setq result (evil-ex-complete-command
                        evil-ex-command
                        evil-ex-force
                        predicate flag))))
       ;; complete argument
       ((memq 'argument context)
        (let ((arg (or evil-ex-argument "")))
          (setq start (or (get-text-property
                           0 'ex-index arg) (point))
                prefix (buffer-substring prompt start))
          (if boundaries
              (setq boundaries (cons 'boundaries
                                     (cons (- start prompt)
                                           (length (cdr flag)))))
            (setq result (evil-ex-complete-argument
                          evil-ex-command
                          arg
                          predicate flag))))))
      ;; return result
      (cond
       (boundaries)
       ((null result)
        nil)
       ((eq result t))
       ((stringp result)
        (if flag result
          (concat prefix result)))
       ((listp result)
        (if flag result
          (mapcar #'(lambda (r) (concat prefix r)) result)))
       (t
        (error "Completion returned unexpected value"))))))

(defun evil-ex-complete-command (cmd force predicate flag)
  "Called to complete a command."
  (cond
   (force
    (let ((pred #'(lambda (x)
                    (when (or (null predicate)
                              (funcall predicate x))
                      (evil-ex-command-force-p x)))))
      (cond
       ((eq flag nil)
        (try-completion cmd evil-ex-commands pred))
       ((eq flag t)
        (all-completions cmd evil-ex-commands pred))
       ((eq flag 'lambda)
        (test-completion cmd evil-ex-commands pred)))))
   (t
    (cond
     ((eq flag nil)
      (let ((result (try-completion cmd evil-ex-commands predicate)))
        (if (and (eq result t) (evil-ex-command-force-p cmd))
            cmd
          result)))
     ((eq flag t)
      (let ((result (all-completions cmd evil-ex-commands predicate))
            new-result)
        (mapc #'(lambda (x)
                  (push x new-result)
                  (when (evil-ex-command-force-p cmd)
                    (push (concat x "!") new-result)))
              result)
        new-result))
     ((eq flag 'lambda)
      (test-completion cmd evil-ex-commands predicate))))))

(defun evil-ex-complete-argument (cmd arg predicate flag)
  "Called to complete the argument of a command.
CMD is the current command. ARG, PREDICATE and FLAG are the
arguments for programmable completion."
  (let* ((binding (evil-ex-completed-binding cmd))
         (arg-type (evil-get-command-property binding :ex-arg))
         (arg-handler (assoc arg-type evil-ex-argument-types)))
    (if arg-handler
        (funcall (cdr arg-handler)
                 'complete
                 arg predicate flag)
      ;; do nothing
      (cond
       ((null arg)
        nil)
       ((null flag)
        nil)
       ((eq flag t)
        (list arg))
       ((eq flag 'lambda))))))

(defun evil-ex-repeat (count)
  "Repeats the last ex command."
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
          (let ((binding (evil-ex-binding evil-ex-command)))
            (unless (eq binding #'evil-ex-repeat)
              (setq hist nil)
              (if evil-ex-expression
                  (eval evil-ex-expression)
                (error "Ex: syntax error")))))))))

(defun evil-ex-call-command (range command argument)
  "Execute the given command COMMAND."
  (let* ((count (when (numberp range) range))
         (range (when (evil-range-p range) range))
         (visual (and range (not (evil-visual-state-p))))
         (force (and (string-match ".!$" command) t))
         (evil-ex-range
          (or range (and count (evil-ex-range count count))))
         (evil-ex-command (evil-ex-completed-binding command))
         (evil-ex-force (and force t))
         (evil-ex-argument (copy-sequence argument))
         (evil-this-type (evil-type evil-ex-range))
         (current-prefix-arg count)
         (prefix-arg current-prefix-arg))
    (when (stringp evil-ex-argument)
      (set-text-properties
       0 (length evil-ex-argument) nil evil-ex-argument))
    (when visual
      (evil-visual-select (evil-range-beginning evil-ex-range)
                          (evil-range-end evil-ex-range)
                          (evil-type evil-ex-range 'line) -1))
    (when (evil-visual-state-p)
      (evil-visual-pre-command evil-ex-command))
    (unwind-protect
        (call-interactively evil-ex-command)
      (when visual
        (evil-exit-visual-state)))))

(defun evil-ex-address (base &optional offset)
  "Return the line number of BASE plus OFFSET."
  (+ (or base (line-number-at-pos))
     (or offset 0)))

(defun evil-ex-first-line ()
  "Return the line number of the first line."
  1)

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
  "Returns the first and last position of the current range."
  (evil-range
   (evil-line-position beg-line)
   (evil-line-position (or end-line beg-line) -1)
   'line))

(defun evil-ex-full-range ()
  "Return a range encompassing the whole buffer."
  (evil-range (point-min) (point-max) 'line))

(defun evil-ex-marker (marker)
  "Return MARKER's line number in the current buffer.
Signal an error if MARKER is in a different buffer."
  (when (stringp marker)
    (setq marker (aref marker 0)))
  (setq marker (evil-get-marker marker))
  (if (numberp marker)
      (line-number-at-pos marker)
    (error "Ex does not support markers in other files")))

(defun evil-ex-re-fwd (pattern)
  "Search forward for PATTERN.
Returns the line number of the match."
  (save-excursion
    (set-text-properties 0 (length pattern) nil pattern)
    (beginning-of-line 2)
    (and (re-search-forward pattern)
         (line-number-at-pos (1- (match-end 0))))))

(defun evil-ex-re-bwd (pattern)
  "Search backward for PATTERN.
Returns the line number of the match."
  (save-excursion
    (set-text-properties 0 (length pattern) nil pattern)
    (beginning-of-line 0)
    (and (re-search-backward pattern)
         (line-number-at-pos (match-beginning 0)))))

(defun evil-ex-prev-search ()
  (error "Previous search not yet implemented"))

(defun evil-ex-signed-number (sign &optional number)
  "Return a signed number like -3 and +1.
NUMBER defaults to 1."
  (funcall sign (or number 1)))

(defun evil-ex-eval (string &optional start)
  "Evaluate STRING as an Ex command.
START is the start symbol, which defaults to `expression'."
  (let ((form (evil-ex-parse string nil start)))
    (eval form)))

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
        force command)
    (when result
      (setq command (car-safe result)
            string (cdr-safe result))
      ;; parse a following "!" as force only if
      ;; the command has the property :ex-force t
      (when (evil-ex-command-force-p command)
        (setq result (evil-parser string 'force evil-ex-grammar)
              force (or (car-safe result) "")
              string (cdr-safe result)
              command (concat command force)))
      (cons command string))))

(defun evil-ex-command-force-p (command)
  "Whether COMMAND accepts the force argument."
  (let ((binding (evil-ex-completed-binding command t)))
    (when binding
      (evil-get-command-property binding :ex-force))))

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
      (setq pair (cons nil string)))
     ;; token
     ((stringp symbol)
      (save-match-data
        (when (or (eq (string-match symbol string) 0)
                  ;; ignore leading whitespace
                  (and (string-match "^[ \f\t\n\r\v]+" string)
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
                               (and (listp rule)
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
        (let* ((dexp
                #'(lambda (obj)
                    (when (symbolp obj)
                      (let ((str (symbol-name obj)))
                        (when (string-match "\\$\\([0-9]+\\)" str)
                          (string-to-number (match-string 1 str)))))))
               ;; traverse a tree for dollar expressions
               (dval nil)
               (dval
                #'(lambda (obj)
                    (if (listp obj)
                        (mapcar dval obj)
                      (let ((num (funcall dexp obj)))
                        (if num
                            (if (not (listp result))
                                result
                              (if (eq num 0)
                                  `(list ,@result)
                                (nth (1- num) result)))
                          obj))))))
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
           ((funcall dexp func)
            (setq result (funcall dval func)))
           ;; function call
           ((listp func)
            (setq result (funcall dval func)))
           ;; symbol
           (t
            (if (memq symbol '(+ seq))
                (setq result `(,func ,@result))
              (setq result `(,func ,result))))))
        (setcar pair result))))
    ;; weed out incomplete matches
    (when pair
      (if (not greedy) pair
        (if (null (cdr pair)) pair
          ;; ignore trailing whitespace
          (when (string-match "^[ \f\t\n\r\v]*$" (cdr pair))
            (unless syntax (setcdr pair nil))
            pair))))))

(provide 'evil-ex)

;;; evil-ex.el ends here
