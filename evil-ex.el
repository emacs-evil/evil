;;; Ex-mode

(define-key evil-ex-keymap "\t" 'minibuffer-complete)
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
    (append (list pos) range command)))

(defun evil-ex-parse-range (text pos)
  "Start parsing TEXT at position POS for a range of lines.
Returns a list (POS START SEP END) where
POS is the position of the first character after the range,
START is a pair (base . offset) describing the start position,
SEP is either ?, or ?;
END is a pair (base . offset) describing the end position."
  (let* ((start (evil-ex-parse-address text pos))
         (sep (evil-ex-parse-address-sep text (pop start)))
         (end (evil-ex-parse-address text (pop sep)))
         (pos (pop end)))
    (append (list pos)
            (and start (list start))
            sep
            (and end (list end)))))

(defun evil-ex-parse-address (text pos)
  "Start parsing TEXT at position POS for a line number.
Returns a list (POS BASE OFFSET) where
POS is the position of the first character after the range,
BASE is the base offset of the line,
OFF is the relative offset of the line from BASE."
  (let* ((base (evil-ex-parse-address-base text pos))
         (off (evil-ex-parse-address-offset text (pop base)))
         (pos (pop off)))
    (list pos (car-safe base) (car-safe off))))

(defun evil-ex-parse-address-base (text pos)
  "Start parsing TEXT at position POS for a base address of a line.
Returns a list (POS ADDR) where
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
   ((>= pos (length text)) (list pos nil))

   ((= pos (or (string-match "[0-9]+" text pos) -1))
    (list (match-end 0)
          (string-to-number (match-string 0 text))))

   (t
    (let ((c (aref text pos)))
      (cond
       ((= c ?$)
        (list (1+ pos) 'last-line))
       ((= c ?\%)
        (list (1+ pos) 'all))
       ((= c ?.)
        (list (1+ pos) 'current-line))
       ((and (= c ?')
             (< pos (1- (length text))))
        (list (+ 2 pos) `(mark ,(aref text (1+ pos)))))
       ((and (= (aref text pos) ?\\)
             (< pos (1- (length text))))
        (let ((c2 (aref text (1+ pos))))
          (cond
           ((= c2 ?/) (list (+ 2 pos) 'next-of-prev-search))
           ((= c2 ??) (list (+ 2 pos) 'prev-of-prev-search))
           ((= c2 ?&) (list (+ 2 pos) 'next-of-prev-subst))
           (t (signal 'ex-parse '("Unexpected symbol after ?\\"))))))
       ((= (aref text pos) ?/)
        (if (string-match "\\([^/]+\\|\\\\.\\)\\(?:/\\|$\\)"
                          text (1+ pos))
            (list (match-end 0)
                  (cons 're-fwd (match-string 1 text)))
          (signal 'ex-parse '("Invalid regular expression"))))
       ((= (aref text pos) ??)
        (if (string-match "\\([^?]+\\|\\\\.\\)\\(?:?\\|$\\)"
                          text (1+ pos))
            (list (match-end 0)
                  (cons 're-bwd (match-string 1 text)))
          (signal 'ex-parse '("Invalid regular expression"))))
       (t
        (list pos nil)))))))

(defun evil-ex-parse-address-sep (text pos)
  "Start parsing TEXT at position POS for an address separator.
Returns a list (POS SEP) where
POS is the position of the first character after the separator,
SEP is either ?; or ?,."
  (if (>= pos (length text))
      (list pos nil)
    (let ((c (aref text pos)))
      (if (member c '(?\, ?\;))
          (list (1+ pos) c)
        (list pos nil)))))

(defun evil-ex-parse-address-offset (text pos)
  "Parses `text' starting at `pos' for an offset, returning a two values,
the offset and the new position."
  (let ((off nil))
    (while (= pos (or (string-match "\\([-+]\\)\\([0-9]+\\)?" text pos) -1))
      (if (string= (match-string 1 text) "+")
          (setq off (+ (or off 0) (if (match-beginning 2)
                                      (string-to-number (match-string 2 text))
                                    1)))

        (setq off (- (or off 0) (if (match-beginning 2)
                                    (string-to-number (match-string 2 text))
                                  1))))
      (setq pos (match-end 0)))
    (list pos off)))

(defun evil-ex-parse-command (text pos)
  (if (and (string-match "\\([a-zA-Z_-]+\\)\\(!\\)?" text pos)
           (= (match-beginning 0) pos))
      (list (match-end 0)
            (match-string 1 text)
            (and (match-beginning 2) t))
    (list pos nil nil)))

(defun evil-ex-complete (cmdline predicate flag)
  "Called to complete an object in the ex-buffer."
  (let* ((result (evil-ex-split cmdline))
         (pos (+ (minibuffer-prompt-end) (pop result)))
         (start (pop result))
         (sep (pop result))
         (end (pop result))
         (cmd (pop result))
         (force (pop result)))
    (if (or (= pos (point))
            (and force (= pos (1+ (point)))))
        (evil-ex-complete-command cmd force predicate flag)
      nil)))

(defun evil-ex-complete-command (cmd force predicate flag)
  (cond
   ((eq flag nil)
    (try-completion cmd evil-ex-commands predicate))
   ((eq flag t)
    (all-completions cmd evil-ex-commands predicate))
   ((eq flag 'lambda)
    (test-completion cmd evil-ex-commands predicate))))

(defun evil-ex-call-current-command ()
  "Execute the given command COMMAND."
  (let ((completed-command (try-completion evil-ex-current-cmd evil-ex-commands nil)))
    (when (eq completed-command t)
      (setq completed-command evil-ex-current-cmd))
    (let ((cmd (assoc completed-command evil-ex-commands)))
      (while (stringp (cdr-safe cmd))
        (setq cmd (assoc (cdr cmd) evil-ex-commands)))
      (if (and cmd (commandp (cdr cmd)))
          (call-interactively (cdr cmd))
        (error "Unknown command %s" evil-ex-current-cmd)))))

(defun evil-ex-read-command (&optional initial-input)
  "Starts ex-mode."
  (interactive)
  (let ((evil-ex-current-buffer (current-buffer)))
    (let ((minibuffer-local-completion-map evil-ex-keymap))
      (let ((result (completing-read ":" 'evil-ex-complete nil nil initial-input  'evil-ex-history)))
        (when (and result
                   (not (zerop (length result))))
          (let* ((ret (evil-ex-split result))
                 (evil-ex-current-cmd (nth 4 ret))
                 (evil-ex-current-arg (substring result (nth 0 ret))))
            (when (and (> (length evil-ex-current-arg) 0)
                       (= (aref evil-ex-current-arg 0) ? ))
              (setq evil-ex-current-arg (substring evil-ex-current-arg 1)))
            (evil-ex-call-current-command)))))))

(defun evil-ex-file-name ()
  "Returns the current argument as file-name."
  evil-ex-current-arg)

(defun evil-write (file-name)
  "Saves the current buffer to FILE-NAME."
  (interactive (list (evil-ex-file-name)))
  (error "Not yet implemened: WRITE <%s>" file-name))

(provide 'evil-ex)

;;; evil-ex.el ends here
