;;; Ex-mode

;; TODO: Emacs 22 completion-boundaries

(require 'evil-common)
(require 'evil-visual)

(define-key evil-ex-keymap "\d" #'evil-ex-delete-backward-char)
(define-key evil-ex-keymap "\t" #'evil-ex-complete)
(define-key evil-ex-keymap "?" nil)

(defun evil-ex-state-p ()
  "Return t iff ex mode is currently active."
  (and evil-ex-current-buffer t))

(defun evil-ex-delete-backward-char ()
  "Closes the minibuffer if called with empty minibuffer content, otherwise behaves like `delete-backward-char'."
  (interactive)
  (call-interactively
   (if (zerop (length (minibuffer-contents)))
       #'abort-recursive-edit
     #'delete-backward-char)))

(defun evil-ex-define-cmd (cmd function)
  "Binds the function FUNCTION to the command CMD."
  (evil-add-to-alist 'evil-ex-commands cmd function))

(defmacro evil-ex-define-argument-type (arg-type args &rest body)
  "Defines a new handler for argument-type ARG-TYPE."
  (declare (indent defun)
           (debug (&define symbolp lambda-list
                           [&optional ("interactive" [&rest form])]
                           def-body)))
  (let ((name (intern (concat "evil-ex-argument-handler-" (symbol-name arg-type)))))
    `(progn
       (defun ,name ,args ,@body)
       (evil-add-to-alist 'evil-ex-arg-types-alist ',arg-type ',name))))

(defun evil-ex-find-symbol (lst symbol)
  "Returns non-nil if LST contains SYMBOL somewhere in a sublist."
  (catch 'done
    (dolist (elm lst)
      (when (or (eq elm symbol)
                (and (listp elm)
                     (evil-ex-find-symbol elm symbol)))
        (throw 'done t)))
    nil))

(defun evil-ex-message (info)
  "Shows an INFO message after the current minibuffer content."
  (when info
    (let ((txt (concat " [" info "]"))
          after-change-functions
          before-change-functions)
      (put-text-property 0 (length txt) 'face 'evil-ex-info txt)
      (minibuffer-message txt))))

(defun evil-ex-split (text)
  "Splits an ex command line in range, command and argument.
Returns a list (POS START SEP END CMD FORCE) where
POS is the first character after the command,
START is a pair (BEG . END) of indices of the start position,
SEP is either ?\, or ?\; separating both range positions,
END is a pair (BEG . END) of indices of the end position,
CMD is a pair (BEG . END) of indices of the command,
FORCE is non-nil if an exclamation mark follows the command."
  (let* ((range (evil-ex-parse-range text 0))
         (command (evil-ex-parse-command text (pop range)))
         (pos (pop command)))
    (cons pos (append range command))))

(defun evil-ex-parse-range (text pos)
  "Start parsing TEXT at position POS for a range of lines.
Returns (POS) if no range has been parsed or a pair (POS . (START
SEP END)) where POS is the position of the first character after
the range, START is nil or a pair (base . offset) describing the
start position, SEP is either ?,, ?; or nil and END is nil or a
pair (base . offset) describing the end position."
  (let* ((start (evil-ex-parse-address text pos))
         (sep (evil-ex-parse-address-sep text (pop start)))
         (end (evil-ex-parse-address text (pop sep)))
         (pos (pop end)))
    (cons pos
          (and (or start sep pos)
               (list start sep end)))))

(defun evil-ex-parse-address (text pos)
  "Start parsing TEXT at position POS for a line number.
Returns a list (POS) if no address has been parsed or (POS BASE
OFFSET) where POS is the position of the first character after
the range, BASE is the base offset of the line, OFF is the
relative offset of the line from BASE."
  (let* ((base (evil-ex-parse-address-base text pos))
         (off (evil-ex-parse-address-offset text (pop base)))
         (pos (pop off)))
    (cons pos (and (or base off) (cons base off)))))

(defun evil-ex-parse-address-base (text pos)
  "Start parsing TEXT at position POS for a base address of a line.
Returns a list (POS) if no base address has been parsed or a
pair (POS . ADDR) where
POS is the position of the first character after the address,
ADDR is the number of the line.
ADDR can be either
* a number, corresponding to the absolute line number
* 'last-line,
* 'current-line,
* 'all which specifies the special range selecting all lines,
* '(re-fwd RE) a regular expression for forward search,
* '(re-bwd RE) a regular expression for backward search,
* '(mark CHAR) a mark."
  (cond
   ((>= pos (length text)) (cons pos nil))

   ((= pos (or (string-match "[0-9]+" text pos) -1))
    (cons (match-end 0)
          (string-to-number (match-string 0 text))))

   (t
    (let ((c (aref text pos)))
      (cond
       ((= c ?$)
        (cons (1+ pos) 'last-line))
       ((= c ?\%)
        (cons (1+ pos) 'all))
       ((= c ?.)
        (cons (1+ pos) 'current-line))
       ((and (= c ?')
             (< pos (1- (length text))))
        (cons (+ 2 pos) `(mark ,(aref text (1+ pos)))))
       ((and (= (aref text pos) ?\\)
             (< pos (1- (length text))))
        (let ((c2 (aref text (1+ pos))))
          (cond
           ((= c2 ?/) (cons (+ 2 pos) 'next-of-prev-search))
           ((= c2 ??) (cons (+ 2 pos) 'prev-of-prev-search))
           ((= c2 ?&) (cons (+ 2 pos) 'next-of-prev-subst))
           (t (signal 'ex-parse '("Unexpected symbol after ?\\"))))))
       ((= (aref text pos) ?/)
        (if (string-match "\\([^/]+\\|\\\\.\\)\\(?:/\\|$\\)"
                          text (1+ pos))
            (cons (match-end 0)
                  (cons 're-fwd (match-string 1 text)))
          (signal 'ex-parse '("Invalid regular expression"))))
       ((= (aref text pos) ??)
        (if (string-match "\\([^?]+\\|\\\\.\\)\\(?:?\\|$\\)"
                          text (1+ pos))
            (cons (match-end 0)
                  (cons 're-bwd (match-string 1 text)))
          (signal 'ex-parse '("Invalid regular expression"))))
       (t
        (cons pos nil)))))))

(defun evil-ex-parse-address-sep (text pos)
  "Start parsing TEXT at position POS for an address separator.
Returns a list (POS) if no separator has been parsed or a
list (POS SEP) where
POS is the position of the first character after the separator,
SEP is either ?;, ?,. or nil if no separator has been parsed"
  (if (>= pos (length text))
      (cons pos nil)
    (let ((c (aref text pos)))
      (if (member c '(?\, ?\;))
          (cons (1+ pos) c)
        (cons pos nil)))))

(defun evil-ex-parse-address-offset (text pos)
  "Parses TEXT starting at POS for an offset.
Returns a list (POS) if no offset has been parsed or a list (POS
OFF) where
POS is the position of the first character after the offset,
OFF is the numerical offset."
  (let ((off nil))
    (while (= pos (or (string-match "\\([-+]\\)\\([0-9]+\\)?" text pos) -1))
      (setq off (funcall (if (string= (match-string 1 text) "+") #'+ #'-)
                         (or off 0)
                         (if (match-beginning 2)
                             (string-to-number (match-string 2 text))
                           1)))
      (setq pos (match-end 0)))
    (cons pos off)))

(defun evil-ex-parse-command (text pos)
  "Parses TEXT starting at POS for a command.
Returns a list (POS CMD FORCE) where
POS is the position of the first character after the separator,
CMD is the parsed command,
FORCE is non-nil if and only if an exclamation followed the command."
  (if (and (string-match "\\([a-zA-Z_-]+\\)\\(!\\)?" text pos)
           (= (match-beginning 0) pos))
      (list (match-end 0)
            (match-string 1 text)
            (and (match-beginning 2) t))
    (list pos nil nil)))

(defun evil-ex-complete ()
  "Starts ex minibuffer completion while temporarily disabling update functions."
  (interactive)
  (let (after-change-functions before-change-functions)
    (minibuffer-complete)))

(defun evil-ex-completion (cmdline predicate flag)
  "Called to complete an object in the ex-buffer."
  (let* ((result (evil-ex-split cmdline))
         (pos (pop result))
         (pnt (+ (minibuffer-prompt-end) pos))
         (start (pop result))
         (sep (pop result))
         (end (pop result))
         (cmd (or (pop result) ""))
         (force (pop result))
         (boundaries (cdr-safe flag)))
    (cond
     ((= (point) pnt)
      (let ((cmdbeg (- pos (length cmd))))
        (if boundaries
            (cons 'boundaries (cons cmdbeg 0))
          (let ((begin (substring cmdline 0 cmdbeg))
                (result (evil-ex-complete-command cmd force predicate flag)))
            (cond
             ((null result) nil)
             ((eq t result) t)
             ((stringp result) (if flag result (concat begin result)))
             ((listp result) (if flag result (mapcar #'(lambda (x) (concat begin x)) result)))
             (t (error "Completion returned unexpected value.")))))))

     ((= (point) (point-max))
      (let ((argbeg (+ pos
                       (if (and (< pos (length cmdline))
                                (= (aref cmdline pos) ?\ ))
                           1
                         0))))
        (if boundaries
            (cons 'boundaries (cons argbeg (length (cdr flag))))
          (let* ((begin (substring cmdline 0 argbeg))
                 (arg (substring cmdline argbeg))
                 (result (evil-ex-complete-argument cmd arg predicate flag)))
            (cond
             ((null result) nil)
             ((eq t result) t)
             ((stringp result) (if flag result (concat begin result)))
             ((listp result) (if flag result (mapcar #'(lambda (x) (concat begin x)) result)))
             (t (error "Completion returned unexpected value."))))))))))

(defun evil-ex-complete-command (cmd force predicate flag)
  "Called to complete a command."
  (let ((has-force #'(lambda (x)
                       (let ((bnd (evil-ex-binding x)))
                         (and bnd (evil-get-command-property bnd :ex-force))))))
    (cond
     (force
      (let ((pred #'(lambda (x)
                      (and (or (null predicate) (funcall predicate x))
                           (funcall has-force x)))))
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
          (if (and (eq result t) (funcall has-force cmd))
              cmd
            result)))
       ((eq flag t)
        (let ((result (all-completions cmd evil-ex-commands predicate))
              new-result)
          (mapc #'(lambda (x)
                    (push x new-result)
                    (when (funcall has-force cmd) (push (concat x "!") new-result)))
                result)
          new-result))
       ((eq flag 'lambda)
        (test-completion cmd evil-ex-commands predicate)))))))

(defun evil-ex-complete-argument (cmd arg predicate flag)
  "Called to complete the argument of a command.
CMD is the current command. ARG, PREDICATE and FLAG are the
arguments for programmable completion."
  (let* ((binding (evil-ex-completed-binding cmd))
         (arg-type (evil-get-command-property binding :ex-arg))
         (arg-handler (assoc arg-type evil-ex-arg-types-alist)))
    (if arg-handler
        (funcall (cdr arg-handler)
                 'complete
                 arg predicate flag)
      ;; do nothing
      (when arg
        (cond
         ((null flag) nil)
         ((eq flag t) (list arg))
         ((eq flag 'lambda) t))))))

(evil-ex-define-argument-type file (flag &rest args)
  "Handles a file argument."
  (when (eq flag 'complete)
    (let ((arg (pop args))
          (predicate (pop args))
          (flag (pop args)))
      (if (null arg)
          default-directory
        (let ((dir (or (file-name-directory arg)
                       (with-current-buffer evil-ex-current-buffer default-directory)))
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
        (let ((buffers (mapcar #'(lambda (buffer) (cons (buffer-name buffer) nil)) (buffer-list t))))
          (cond
           ((null flag)
            (try-completion arg buffers predicate))
           ((eq t flag)
            (all-completions arg buffers predicate))
           ((eq 'lambda flag)
            (test-completion arg buffers predicate))))))))

(defun evil-ex-update-current-command (command)
  "Updates the internal variables representing the content of the ex-buffer."
  (let* ((result (evil-ex-split command))
         (pos (pop result))
         (start (pop result))
         (sep (pop result))
         (end (pop result))
         (cmd (pop result))
         (force (pop result)))
    (setq evil-ex-current-cmd cmd
          evil-ex-current-arg (cond
                               ;; Eat the first space of the argument
                               ((and (> (length command) (1+ pos))
                                     (= (aref command pos) ? ))
                                (substring command (1+ pos)))
                               ((> (length command) pos)
                                (substring command pos)))
          evil-ex-current-cmd-force force
          evil-ex-current-range (and (or start sep end) (list start sep end)))))

(defun evil-ex-update (beg end len)
  "Updates ex-variable in ex-mode when the buffer content changes."
  (let ((oldcmd evil-ex-current-cmd))
    (evil-ex-update-current-command (buffer-substring (minibuffer-prompt-end) (point-max)))
    (let ((bnd (and evil-ex-current-cmd
                    (evil-ex-completed-binding evil-ex-current-cmd))))
      ;; Test the current command if different from the previous command
      (when (and evil-ex-current-cmd
                 (not (equal evil-ex-current-cmd oldcmd)))
        (let ((compl (if (assoc evil-ex-current-cmd evil-ex-commands)
                         (list t)
                       (mapcar #'evil-ex-binding
                               (all-completions evil-ex-current-cmd
                                                evil-ex-commands)))))
          (cond
           ((null compl) (evil-ex-message "Unknown command"))
           ((cdr compl) (evil-ex-message "Incomplete command")))))
      ;; update arg-handler
      (let* ((arg-type (evil-get-command-property bnd :ex-arg))
             (arg-handler (and arg-type
                               (cdr-safe
                                (assoc arg-type
                                       evil-ex-arg-types-alist)))))
        (unless (eq arg-handler evil-ex-current-arg-handler)
          (when evil-ex-current-arg-handler
            (funcall evil-ex-current-arg-handler 'stop))
          (setq evil-ex-current-arg-handler arg-handler)
          (when evil-ex-current-arg-handler
            (funcall evil-ex-current-arg-handler 'start evil-ex-current-arg)))
        (when evil-ex-current-arg-handler
          (funcall evil-ex-current-arg-handler 'update evil-ex-current-arg))))))

(defun evil-ex-binding (command)
  "Returns the final binding of COMMAND."
  (let ((cmd (assoc command evil-ex-commands)))
    (while (stringp (cdr-safe cmd))
      (setq cmd (assoc (cdr cmd) evil-ex-commands)))
    (and cmd (cdr cmd))))

(defun evil-ex-completed-binding (command)
  "Returns the final binding of the completion of COMMAND."
  (let ((completed-command (try-completion command evil-ex-commands nil)))
    (evil-ex-binding (if (eq completed-command t) command completed-command))))

(defun evil-ex-call-current-command ()
  "Execute the given command COMMAND."
  (if (not evil-ex-current-cmd)
      (if (and evil-ex-current-range
               (car evil-ex-current-range)
               (numberp (caar evil-ex-current-range)))
          ;; TODO: we use funcall to avoid the compiler complaining about
          ;;       undefined `evil-goto-line'. We can't require evil-motions.el
          ;;       because this would lead to recursive requires.
          (let ((fn 'evil-goto-line))
            (funcall fn (caar evil-ex-current-range)))
        (error "Invalid ex-command."))
    (let ((binding (evil-ex-completed-binding evil-ex-current-cmd)))
      (if binding
          (with-current-buffer evil-ex-current-buffer
            (save-excursion
              (let ((range (evil-ex-get-current-range))
                    prefix-arg)
                (when (and (not range)
                           evil-ex-current-range
                           (car evil-ex-current-range)
                           (numberp (caar evil-ex-current-range)))
                  (setq prefix-arg (caar evil-ex-current-range)))
                (call-interactively binding))))
        (error "Unknown command %s" evil-ex-current-cmd)))))

(defun evil-ex-range ()
  "Returns the first and last position of the current range."
  (let ((rng (evil-ex-get-current-range)))
    (when rng
      (evil-range
       (save-excursion
         (goto-char (point-min)) (forward-line (1- (car rng)))
         (line-beginning-position))
       (save-excursion
         (goto-char (point-min)) (forward-line (1- (cdr rng)))
         (min (point-max) (1+ (line-end-position))))
       'line))))

(defun evil-ex-get-current-range ()
  "Returns the line-numbers of the current range. A range is
returned if and only if a range separator is specified, otherwise
the number is interpreted as prefix argument (i.e., as numeric
count) in which case this function returns nil."
  (when (and evil-ex-current-range)
    (cond
     ((and (car evil-ex-current-range)
           (eq (car (car evil-ex-current-range)) 'all))
      (cons (point-min) (point-max)))
     ((cadr evil-ex-current-range)
      (let ((beg (evil-ex-get-line (car evil-ex-current-range))))
        (save-excursion
          (when (equal (cadr evil-ex-current-range) ?\;)
            (goto-char (point-min))
            (forward-line (1- beg)))
          (cons beg (evil-ex-get-line (nth 2 evil-ex-current-range)))))))))

(defun evil-ex-get-line (address)
  "Returns the line represented by ADDRESS."
  (if (not address)
      (line-number-at-pos)
    (let ((base (car address))
          (offset (or (cdr address) 0)))
      (+ (or offset 0)
         (if (integerp base) base
           (cond
            ((eq base nil) (line-number-at-pos))
            ((eq base 'abs) (cdr base))

            ;; TODO: (1- ...) may be wrong if the match is the empty string
            ((eq base 're-fwd)
             (save-excursion
               (beginning-of-line 2)
               (and (re-search-forward (cdr base))
                    (line-number-at-pos (1- (match-end 0))))))

            ((eq base 're-bwd)
             (save-excursion
               (beginning-of-line 0)
               (and (re-search-backward (cdr base))
                    (line-number-at-pos (match-beginning 0)))))

            ((eq base 'current-line) (line-number-at-pos (point)))
            ((eq base 'first-line) (line-number-at-pos (point-min)))
            ((eq base 'last-line) (line-number-at-pos (point-max)))
            ((and (consp base) (eq (car base) 'mark))
             (let ((m (evil-get-marker (cadr base))))
               (cond
                ((eq m nil) (error "Marker <%c> not defined" (cadr base)))
                ((consp m) (error "Ex-mode ranges do not support markers in other files"))
                (t (line-number-at-pos m)))))
            ((eq base 'next-of-prev-search) (error "Next-of-prev-search not yet implemented."))
            ((eq base 'prev-of-prev-search) (error "Prev-of-prev-search not yet implemented."))
            ((eq base 'next-of-prev-subst) (error "Next-of-prev-subst not yet implemented."))
            (t (error "Invalid address: %s" address))))))))

;;; TODO: extensions likes :p :~ <cfile> ...
(defun evil-ex-replace-special-filenames (file-name)
  "Replaces % by the current file-name, # by the alternate file-name in FILE-NAME."
  (let ((current-fname (buffer-file-name))
        (alternate-fname (and (other-buffer) (buffer-file-name (other-buffer)))))
    (when current-fname
      (setq file-name
            (replace-regexp-in-string "\\(^\\|[^\\\\]\\)\\(%\\)"
                                      current-fname
                                      file-name
                                      t
                                      t
                                      2)))
    (when alternate-fname
      (setq file-name
            (replace-regexp-in-string "\\(^\\|[^\\\\]\\)\\(#\\)"
                                      alternate-fname
                                      file-name
                                      t
                                      t
                                      2)))
    (setq file-name
          (replace-regexp-in-string "\\\\\\([#%]\\)"
                                    "\\1"
                                    file-name
                                    t)))
  file-name)

(defun evil-ex-file-arg ()
  "Returns the current ex-argument as file name.
This function interprets special file-names like # and %."
  (unless (or (null evil-ex-current-arg)
              (zerop (length evil-ex-current-arg)))
    (evil-ex-replace-special-filenames evil-ex-current-arg)))

(defun evil-ex-setup ()
  "Initializes ex minibuffer."
  (add-hook 'after-change-functions #'evil-ex-update nil t)
  (add-hook 'minibuffer-exit-hook #'evil-ex-teardown)
  (when evil-ex-last-cmd
    (add-hook 'pre-command-hook #'evil-ex-remove-default))
  (remove-hook 'minibuffer-setup-hook #'evil-ex-setup))

(defun evil-ex-teardown ()
  "Deinitializes ex minibuffer."
  (remove-hook 'minibuffer-exit-hook #'evil-ex-teardown)
  (remove-hook 'after-change-functions #'evil-ex-update t)
  (when evil-ex-current-arg-handler
    (funcall evil-ex-current-arg-handler 'stop)))

(defun evil-ex-remove-default ()
  (delete-minibuffer-contents)
  (remove-hook 'pre-command-hook #'evil-ex-remove-default))

(defun evil-ex-read-command (&optional initial-input)
  "Starts ex-mode."
  (interactive (and (evil-visual-state-p) '("'<,'>")))
  (let ((evil-ex-current-buffer (current-buffer))
        (minibuffer-local-completion-map evil-ex-keymap)
        evil-ex-current-arg-handler
        evil-ex-info-string
        (evil-ex-last-cmd (and (not initial-input)
                               (car-safe evil-ex-history))))
    (add-hook 'minibuffer-setup-hook #'evil-ex-setup)
    (let ((result (completing-read ":"
                                   #'evil-ex-completion
                                   nil
                                   nil
                                   (or initial-input
                                       (and evil-ex-last-cmd
                                            (concat "(default: " evil-ex-last-cmd ") ")))
                                   'evil-ex-history
                                   evil-ex-last-cmd
                                   t)))
      (when (and result (not (zerop (length result))))
        (evil-ex-update-current-command result)
        (evil-ex-call-current-command)))))

(provide 'evil-ex)

;;; evil-ex.el ends here
