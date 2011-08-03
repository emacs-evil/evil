;;; Ex-mode

;; TODO: Emacs 22 completion-boundaries

(require 'evil-common)
(require 'evil-vars)

(define-key evil-ex-keymap "\t" 'evil-ex-complete)
(define-key evil-ex-keymap [return] 'exit-minibuffer)
(define-key evil-ex-keymap (kbd "RET") 'exit-minibuffer)
(define-key evil-ex-keymap (kbd "C-j") 'exit-minibuffer)
(define-key evil-ex-keymap (kbd "C-g") 'abort-recursive-edit)
(define-key evil-ex-keymap [up] 'previous-history-element)
(define-key evil-ex-keymap [down] 'next-history-element)
(define-key evil-ex-keymap "\d" 'delete-backward-char)


(defun evil-ex-define-cmd (cmd function)
  "Binds the function FUNCTION to the command CMD."
  (evil-add-to-alist 'evil-ex-commands cmd function))

(defun evil-find-symbol (lst symbol)
  "Returns non-nil if LST contains SYMBOL somewhere in a sublist."
  (catch 'done
    (dolist (elm lst)
      (when (or (eq elm symbol)
                (and (listp elm)
                     (evil-find-symbol elm symbol)))
        (throw 'done t)))
    nil))

;; TODO: this test is not very robust, could be done better
(defun evil-ex-has-force (cmd)
  "Returns non-nil iff the command CMD checks for a ex-force argument in its interactive list.
The test for the force-argument should be done by checking the
value of the variable `evil-ex-current-cmd-force'."
  (evil-find-symbol (interactive-form cmd) 'evil-ex-current-cmd-force))

(defun evil-ex-has-file-argument (cmd)
  "Returns non-nil iff the command CMD checks for an `evil-ex-file-name' argument in its interactive list."
  (evil-find-symbol (interactive-form cmd) 'evil-ex-file-name))

(defun evil-ex-has-buffer-argument (cmd)
  "Returns non-nil iff the command CMD checks for an `evil-ex-buffer-name' argument in its interactive list."
  (evil-find-symbol (interactive-form cmd) 'evil-ex-buffer-name))

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
      (if boundaries
          (cons 'boundaries (cons (- pos (length cmd)) 0))
        (evil-ex-complete-command cmd force predicate flag)))
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
                         (and bnd (evil-ex-has-force bnd))))))
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
  (let ((binding (evil-ex-completed-binding cmd)))
    (cond
     ((evil-ex-has-file-argument binding)
      (evil-ex-complete-file-argument arg predicate flag))
     ((evil-ex-has-buffer-argument binding)
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
    (if rng
        (list (save-excursion
                (goto-char (point-min)) (forward-line (1- (car rng)))
                (line-beginning-position))
              (save-excursion
                (goto-char (point-min)) (forward-line (1- (cdr rng)))
                (min (point-max) (1+ (line-end-position)))))
      (list nil nil))))

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


(defun evil-ex-read (prompt
                     collection
                     update
                     &optional
                     require-match
                     initial
                     hist
                     default
                     inherit-input-method)
  "Starts a completing ex minibuffer session.
The parameters are the same as for `completing-read' but an
additional UPDATE function can be given which is called as an
hook of after-change-functions."
  (let ((evil-ex-current-buffer (current-buffer)))
    (let ((minibuffer-local-completion-map evil-ex-keymap)
          (evil-ex-update-function update)
          (evil-ex-info-string nil))
      (add-hook 'minibuffer-setup-hook #'evil-ex-setup)
      (completing-read prompt collection nil require-match initial hist default inherit-input-method))))

(defun evil-ex-setup ()
  "Initializes ex minibuffer."
  (when evil-ex-update-function
    (add-hook 'after-change-functions evil-ex-update-function nil t))
  (add-hook 'minibuffer-exit-hook #'evil-ex-teardown)
  (remove-hook 'minibuffer-setup-hook #'evil-ex-setup))

(defun evil-ex-teardown ()
  "Deinitializes ex minibuffer."
  (remove-hook 'minibuffer-exit-hook #'evil-ex-teardown)
  (when evil-ex-update-function
    (remove-hook 'after-change-functions evil-ex-update-function t)
    (funcall evil-ex-update-function (point-min) (point-max) 0)))

(defun evil-ex-read-command (&optional initial-input)
  "Starts ex-mode."
  (interactive)
  (let ((evil-ex-current-buffer (current-buffer))
        (result (evil-ex-read ":" 'evil-ex-completion 'evil-ex-update nil initial-input  'evil-ex-history)))
    (when (and result (not (zerop (length result))))
      (evil-ex-call-current-command))))


(defun evil-ex-file-name ()
  "Returns the current argument as file-name."
  evil-ex-current-arg)

(defun evil-ex-buffer-name ()
  "Returns the current argument as buffer-name."
  evil-ex-current-arg)


(defun evil-write (file-name &optional beg end force)
  "Saves the current buffer or the region from BEG to END to FILE-NAME.
If the argument FORCE is non-nil, the file will be overwritten if
already existing."
  (interactive (append
                (list (evil-ex-file-name))
                (evil-ex-range)
                (list evil-ex-current-cmd-force)))
  "Saves the lines from `begin' to `end' to file `file-name'."
  (when (null file-name)
    (setq file-name (buffer-file-name))
    (unless file-name
      (error "Please specify a file-name for this buffer!")))

  (cond
   ((and (null beg)
         (string= file-name (buffer-file-name)))
    (save-buffer))
   ((and (null beg)
         (null (buffer-file-name)))
    (write-file file-name (not force)))
   (t
    (write-region beg end file-name nil nil nil (not force)))))

(defun evil-write-all (force)
  "Saves all buffers."
  (interactive (list evil-ex-current-cmd-force))
  (save-some-buffers force))

(defun evil-edit (file)
  "Visits a certain file."
  (interactive (list (evil-ex-file-name)))
  (if file
      (find-file file)
    (when (buffer-file-name)
      (find-file (buffer-file-name)))))

(defun evil-show-buffers ()
  "Shows the buffer-list."
  (interactive)
  (let (message-truncate-lines message-log-max)
    (message "%s"
             (mapconcat #'buffer-name (buffer-list) "\n"))))

(defun evil-buffer (buffer)
  "Switches to another buffer."
  (interactive (list (evil-ex-buffer-name)))
  (if buffer
      (when (or (get-buffer buffer)
                (y-or-n-p (format "No buffer with name \"%s\" exists. Create new buffer? " buffer)))
        (switch-to-buffer buffer))
    (switch-to-buffer (other-buffer))))

(defun evil-next-buffer (&optional count)
  "Goes to the `count'-th next buffer in the buffer list."
  (interactive "p")
  (dotimes (i (or count 1))
    (next-buffer)))

(defun evil-prev-buffer (&optional count)
  "Goes to the `count'-th prev buffer in the buffer list."
  (interactive "p")
  (dotimes (i (or count 1))
    (previous-buffer)))

(defun evil-split-buffer (buffer)
  "Splits window and switches to another buffer."
  (interactive (list (evil-ex-buffer-name)))
  (evil-window-split)
  (evil-buffer buffer))

(defun evil-split-next-buffer (&optional count)
  "Splits window and goes to the `count'-th next buffer in the buffer list."
  (interactive "p")
  (evil-window-split)
  (evil-next-buffer count))

(defun evil-split-prev-buffer (&optional count)
  "Splits window and goes to the `count'-th prev buffer in the buffer list."
  (interactive "p")
  (evil-window-split)
  (evil-prev-buffer count))

(defun evil-delete-buffer (buffer &optional force)
  "Deletes a buffer."
  (interactive (list (evil-ex-buffer-name) evil-ex-current-cmd-force))
  (when force
    (if buffer
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
      (set-buffer-modified-p nil)))
  (kill-buffer buffer))

(defun evil-quit (&optional force)
  "Closes the current window, exits Emacs if this is the last window."
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

(defun evil-quit-all (&optional force)
  "Exits Emacs, asking for saving."
  (interactive (list evil-ex-current-cmd-force))
  (if force
      (kill-emacs)
    (save-buffers-kill-emacs)))

(defun evil-save-and-quit ()
  "Exits Emacs, without saving."
  (interactive)
  (save-buffers-kill-emacs 1))

(defun evil-save-and-close (file &optional force)
  "Saves the current buffer and closes the window."
  (interactive (list (evil-ex-file-name) evil-ex-current-cmd-force))
  (evil-write file force)
  (evil-quit))

(provide 'evil-ex)

;;; evil-ex.el ends here
