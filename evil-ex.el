;;; Ex-mode

;; TODO: Emacs 22 completion-boundaries

(require 'evil-common)
(require 'evil-operators)
(require 'evil-vars)

(define-key evil-ex-keymap "\d" #'evil-ex-delete-backward-char)

(defun evil-ex-state-p ()
  "Return t iff ex mode is currently active."
  (and evil-ex-current-buffer t))

(defun evil-ex-delete-backward-char ()
  "Closes the minibuffer if called with empty minibuffer content, otherwise behaves like `delete-backward-char'."
  (interactive)
  (call-interactively
   (if (zerop (length (minibuffer-contents)))
       #'exit-minibuffer
     #'delete-backward-char)))

(defun evil-ex-define-cmd (cmd function)
  "Binds the function FUNCTION to the command CMD."
  (evil-add-to-alist 'evil-ex-commands cmd function))

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
         (cmd (pop result))
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
         (arg-type (evil-get-command-property binding :ex-arg)))
    (cond
     ((eq arg-type 'file)
      (evil-ex-complete-file-argument arg predicate flag))
     ((eq arg-type 'buffer)
      (evil-ex-complete-buffer-argument arg predicate flag))
     (t
      ;; do nothing
      (when arg
        (cond
         ((null flag) nil)
         ((eq flag t) (list arg))
         ((eq flag 'lambda) t)))))))


(defun evil-ex-complete-file-argument (arg predicate flag)
  "Called to complete a file argument."
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
        (eq (file-name-completion fname dir) t))))))


(defun evil-ex-complete-buffer-argument (arg predicate flag)
  "Called to complete a buffer name argument."
  (when arg
    (let ((buffers (mapcar #'(lambda (buffer) (cons (buffer-name buffer) nil)) (buffer-list t))))
      (cond
       ((null flag)
        (try-completion arg buffers predicate))
       ((eq t flag)
        (all-completions arg buffers predicate))
       ((eq 'lambda flag)
        (test-completion arg buffers predicate))))))


(defun evil-ex-update (beg end len)
  "Updates ex-variable in ex-mode when the buffer content changes."
  (let* ((result (evil-ex-split (buffer-substring (minibuffer-prompt-end) (point-max))))
         (pos (+ (minibuffer-prompt-end) (pop result)))
         (start (pop result))
         (sep (pop result))
         (end (pop result))
         (cmd (pop result))
         (force (pop result))
         (oldcmd evil-ex-current-cmd))
    (setq evil-ex-current-cmd cmd
          evil-ex-current-arg (and (> (point-max) pos) (buffer-substring pos (point-max)))
          evil-ex-current-cmd-end (if force (1- pos) pos)
          evil-ex-current-cmd-begin (- evil-ex-current-cmd-end (length cmd))
          evil-ex-current-cmd-force force
          evil-ex-current-range (list start sep end))
    ;; Ensure `evil-ex-current-range' is nil when no range has been given
    (unless (or (car start) (cadr end) sep (car end) (cadr end))
      (setq evil-ex-current-range nil))
    (when (and (> (length evil-ex-current-arg) 0)
               (= (aref evil-ex-current-arg 0) ? ))
      (setq evil-ex-current-arg (substring evil-ex-current-arg 1)))
    (when (and cmd (not (equal cmd oldcmd)))
      (let (compl)
        (if (assoc cmd evil-ex-commands)
            (setq compl (list t))
          (dolist (c (all-completions evil-ex-current-cmd evil-ex-commands))
            (add-to-list 'compl (evil-ex-binding c))))
        (cond
         ((null compl) (evil-ex-message "Unknown command"))
         ((cdr compl) (evil-ex-message "Incomplete command")))))))

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
      (error "Unknown command %s" evil-ex-current-cmd))))

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
  (when (and evil-ex-current-range
           (cadr evil-ex-current-range))
    (let ((beg (evil-ex-get-line (car evil-ex-current-range))))
      (save-excursion
        (when (equal (cadr evil-ex-current-range) ?\;)
          (goto-char (point-min))
          (forward-line (1- beg)))
        (cons beg (evil-ex-get-line (nth 2 evil-ex-current-range)))))))

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

(defun evil-ex-setup ()
  "Initializes ex minibuffer."
  (add-hook 'after-change-functions #'evil-ex-update nil t)
  (add-hook 'minibuffer-exit-hook #'evil-ex-teardown)
  (remove-hook 'minibuffer-setup-hook #'evil-ex-setup))

(defun evil-ex-teardown ()
  "Deinitializes ex minibuffer."
  (remove-hook 'minibuffer-exit-hook #'evil-ex-teardown)
  (remove-hook 'after-change-functions #'evil-ex-update t)
  (evil-ex-update (point-min) (point-max) 0))

(defun evil-ex-read-command (&optional initial-input)
  "Starts ex-mode."
  (interactive (and (evil-visual-state-p) '("'<,'>")))
  (let ((evil-ex-current-buffer (current-buffer))
        (minibuffer-local-completion-map evil-ex-keymap)
        evil-ex-info-string)
    (add-hook 'minibuffer-setup-hook #'evil-ex-setup)
    (let ((result (completing-read ":"
                                   #'evil-ex-completion
                                   nil
                                   nil
                                   initial-input
                                   'evil-ex-history nil t)))
      (when (and result (not (zerop (length result))))
        (evil-ex-call-current-command)))))

(evil-define-operator evil-write (beg end type file-name &optional force)
  "Saves the current buffer or the region from BEG to END to FILE-NAME.
If the argument FORCE is non-nil, the file will be overwritten if
already existing."
  :motion mark-whole-buffer
  :type line
  :repeat nil
  (interactive "<f><!>")
  (when (null file-name)
    (setq file-name (buffer-file-name))
    (unless file-name
      (error "Please specify a file-name for this buffer!")))

  (cond
   ((and (= beg (point-min)) (= end (point-max))
         (string= file-name (buffer-file-name)))
    (save-buffer))
   ((and (= beg (point-min)) (= end (point-max))
         (null (buffer-file-name)))
    (write-file file-name (not force)))
   (t
    (write-region beg end file-name nil nil nil (not force)))))

(evil-define-command evil-write-all (force)
  "Saves all buffers."
  :repeat nil
  (interactive "<!>")
  (save-some-buffers force))

(evil-define-command evil-edit (file)
  "Visits a certain file."
  :repeat nil
  (interactive "<f>")
  (if file
      (find-file file)
    (when (buffer-file-name)
      (find-file (buffer-file-name)))))

(evil-define-command evil-show-buffers ()
  "Shows the buffer-list."
  :repeat nil
  (interactive)
  (let (message-truncate-lines message-log-max)
    (message "%s"
             (mapconcat #'buffer-name (buffer-list) "\n"))))

(evil-define-command evil-buffer (buffer)
  "Switches to another buffer."
  :repeat nil
  (interactive "<b>")
  (if buffer
      (when (or (get-buffer buffer)
                (y-or-n-p (format "No buffer with name \"%s\" exists. Create new buffer? " buffer)))
        (switch-to-buffer buffer))
    (switch-to-buffer (other-buffer))))

(evil-define-command evil-next-buffer (&optional count)
  "Goes to the `count'-th next buffer in the buffer list."
  :repeat nil
  (interactive "p")
  (dotimes (i (or count 1))
    (next-buffer)))

(evil-define-command evil-prev-buffer (&optional count)
  "Goes to the `count'-th prev buffer in the buffer list."
  :repeat nil
  (interactive "p")
  (dotimes (i (or count 1))
    (previous-buffer)))

(evil-define-command evil-split-buffer (buffer)
  "Splits window and switches to another buffer."
  :repeat nil
  (interactive "<b>")
  (evil-window-split)
  (evil-buffer buffer))

(evil-define-command evil-split-next-buffer (&optional count)
  "Splits window and goes to the `count'-th next buffer in the buffer list."
  :repeat nil
  (interactive "p")
  (evil-window-split)
  (evil-next-buffer count))

(evil-define-command evil-split-prev-buffer (&optional count)
  "Splits window and goes to the `count'-th prev buffer in the buffer list."
  :repeat nil
  (interactive "p")
  (evil-window-split)
  (evil-prev-buffer count))

(evil-define-command evil-delete-buffer (buffer &optional force)
  "Deletes a buffer."
  (interactive "<b><!>")
  (when force
    (if buffer
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
      (set-buffer-modified-p nil)))
  (kill-buffer buffer))

(evil-define-command evil-quit (&optional force)
  "Closes the current window, exits Emacs if this is the last window."
  :repeat nil
  (interactive (list evil-ex-current-cmd-force))
  (condition-case nil
      (delete-window)
    (error
     (condition-case nil
         (delete-frame)
       (error
        (if force
            (kill-emacs)
          (save-buffers-kill-emacs)))))))

(evil-define-command evil-quit-all (&optional force)
  "Exits Emacs, asking for saving."
  :repeat nil
  (interactive "<!>")
  (if force
      (kill-emacs)
    (save-buffers-kill-emacs)))

(evil-define-command evil-save-and-quit ()
  "Exits Emacs, without saving."
  (interactive)
  (save-buffers-kill-emacs 1))

(evil-define-command evil-save-and-close (file &optional force)
  "Saves the current buffer and closes the window."
  :repeat nil
  (interactive "<f><!>")
  (evil-write file force)
  (evil-quit))

(provide 'evil-ex)

;;; evil-ex.el ends here
