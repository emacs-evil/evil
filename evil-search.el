;;;; Search

(require 'evil-common)
(require 'evil-motions)
(require 'evil-ex)

(evil-define-motion evil-search-forward ()
  (format "Search forward for user-entered text.
Searches for regular expression if `evil-regexp-search' is t.%s"
          (if (and (fboundp 'isearch-forward)
                   (documentation 'isearch-forward))
              (format "\n\nBelow is the documentation string \
for `isearch-forward',\nwhich lists available keys:\n\n%s"
                      (documentation 'isearch-forward)) ""))
  :type exclusive
  (evil-search-incrementally t evil-regexp-search))

(evil-define-motion evil-search-backward ()
  (format "Search backward for user-entered text.
Searches for regular expression if `evil-regexp-search' is t.%s"
          (if (and (fboundp 'isearch-forward)
                   (documentation 'isearch-forward))
              (format "\n\nBelow is the documentation string \
for `isearch-forward',\nwhich lists available keys:\n\n%s"
                      (documentation 'isearch-forward)) ""))
  :jump t
  :type exclusive
  (evil-search-incrementally nil evil-regexp-search))

(evil-define-motion evil-search-next (count)
  "Repeat the last search."
  :jump t
  :type exclusive
  (dotimes (var (or count 1))
    (evil-search (if evil-regexp-search
                     (car-safe regexp-search-ring)
                   (car-safe search-ring))
                 isearch-forward evil-regexp-search)))

(evil-define-motion evil-search-previous (count)
  "Repeat the last search in the opposite direction."
  :jump t
  :type exclusive
  (dotimes (var (or count 1))
    (evil-search (if evil-regexp-search
                     (car-safe regexp-search-ring)
                   (car-safe search-ring))
                 (not isearch-forward) evil-regexp-search)))

(evil-define-motion evil-search-symbol-backward (count)
  "Search backward for symbol under point."
  :jump t
  :type exclusive
  (dotimes (var (or count 1))
    (evil-search-symbol nil)))

(evil-define-motion evil-search-symbol-forward (count)
  "Search forward for symbol under point."
  :jump t
  :type exclusive
  (dotimes (var (or count 1))
    (evil-search-symbol t)))

(evil-define-motion evil-goto-definition ()
  "Go to definition or first occurrence of symbol under point."
  :jump t
  :type exclusive
  (let* ((string (evil-find-symbol t))
         (search (if evil-regexp-search (regexp-quote string) string))
         ientry ipos)
    ;; load imenu if available
    (unless (featurep 'imenu)
      (condition-case nil
          (require 'imenu)
        (error nil)))
    (if (null string)
        (error "No symbol under cursor")
      (setq isearch-forward t)
      ;; if imenu is available, try it
      (cond
       ((fboundp 'imenu--make-index-alist)
        (condition-case nil
            (setq ientry (imenu--make-index-alist))
          (error nil))
        (setq ientry (assoc string ientry))
        (setq ipos (cdr ientry))
        (unless (markerp ipos)
          (setq ipos (cadr ientry)))
        (cond
         ;; imenu found a position, so go there and
         ;; highlight the occurrence
         ((and (markerp ipos)
               (eq (marker-buffer ipos) (current-buffer)))
          (evil-search search t evil-regexp-search ipos))
         ;; imenu failed, so just go to first occurrence in buffer
         (t
          (evil-search search t evil-regexp-search (point-min)))))
       ;; no imenu, so just go to first occurrence in buffer
       (t
        (evil-search search t evil-regexp-search (point-min)))))))

(defun evil-search-incrementally (forward regexp-p)
  "Search incrementally for user-entered text."
  (let ((evil-search-prompt (evil-search-prompt forward))
        (isearch-search-fun-function 'evil-isearch-function)
        (point (point))
        search-nonincremental-instead)
    (setq isearch-forward forward)
    (evil-save-echo-area
      (if forward
          (isearch-forward regexp-p)
        (isearch-backward regexp-p))
      (when (and (eq point (point))
                 (not (string= isearch-string "")))
        (if forward
            (isearch-repeat-forward)
          (isearch-repeat-backward))
        (isearch-exit))
      ;; always position point at the beginning of the match
      (when (and forward isearch-other-end)
        (goto-char isearch-other-end))
      (evil-flash-search-pattern
       (evil-search-message isearch-string forward)))))

(defun evil-flash-search-pattern (string &optional all)
  "Flash last search matches for duration of `evil-flash-delay'.
If ALL is non-nil, flash all matches. STRING is a string
to display in the echo area."
  (let ((lazy-highlight-initial-delay 0)
        (isearch-search-fun-function 'evil-isearch-function)
        (isearch-case-fold-search case-fold-search)
        (disable (lambda (&optional arg) (evil-flash-hook t))))
    (when evil-flash-timer
      (cancel-timer evil-flash-timer))
    (unless (or (null string)
                (string= string ""))
      (evil-echo-area-save)
      (evil-echo string)
      (isearch-highlight (match-beginning 0) (match-end 0))
      (when all
        (setq isearch-lazy-highlight-wrapped nil
              isearch-lazy-highlight-start (point)
              isearch-lazy-highlight-end (point))
        (isearch-lazy-highlight-new-loop)
        (unless isearch-lazy-highlight-overlays
          (isearch-lazy-highlight-update)))
      (add-hook 'pre-command-hook 'evil-flash-hook)
      (add-hook 'pre-command-hook 'evil-clean-isearch-overlays)
      (setq evil-flash-timer
            (run-at-time evil-flash-delay nil disable)))))

(defun evil-clean-isearch-overlays ()
  "Clean isearch overlays unless `this-command' is search."
  (remove-hook 'pre-command-hook 'evil-clean-isearch-overlays)
  (unless (memq this-command
                '(evil-search-backward
                  evil-search-forward
                  evil-search-next
                  evil-search-previous
                  evil-search-symbol-backward
                  evil-search-symbol-forward))
    (isearch-clean-overlays)))

(defun evil-flash-hook (&optional force)
  "Disable hightlighting if `this-command' is not search.
Disable anyway if FORCE is t."
  (when (or force
            ;; to avoid flicker, don't disable highlighting
            ;; if the next command is also a search command
            (not (memq this-command
                       '(evil-search-backward
                         evil-search-forward
                         evil-search-next
                         evil-search-previous
                         evil-search-symbol-backward
                         evil-search-symbol-forward))))
    (evil-echo-area-restore)
    (isearch-dehighlight)
    (setq isearch-lazy-highlight-last-string nil)
    (lazy-highlight-cleanup t)
    (when evil-flash-timer
      (cancel-timer evil-flash-timer)))
  (remove-hook 'pre-command-hook 'evil-flash-hook))

(defun evil-search-function (&optional forward regexp-p wrap)
  "Return a search function.
If FORWARD is nil, search backward, otherwise forward.
If REGEXP-P is non-nil, the input is a regular expression.
If WRAP is non-nil, the search wraps around the top or bottom
of the buffer."
  `(lambda (string &optional bound noerror count)
     (let ((start (point))
           (search-fun ',(if regexp-p
                             (if forward
                                 're-search-forward
                               're-search-backward)
                           (if forward
                               'search-forward
                             'search-backward)))
           result)
       (setq result (funcall search-fun string bound
                             ,(if wrap t 'noerror) count))
       (when (and ,wrap (null result))
         (goto-char ,(if forward '(point-min) '(point-max)))
         (unwind-protect
             (setq result (funcall search-fun string bound noerror count))
           (unless result
             (goto-char start))))
       result)))

(defun evil-isearch-function ()
  "Return a search function for use with isearch.
Based on `isearch-regexp' and `isearch-forward'."
  (evil-search-function isearch-forward evil-regexp-search evil-search-wrap))

(defun evil-search (string forward &optional regexp-p start)
  "Search for STRING and highlight matches.
If FORWARD is nil, search backward, otherwise forward.
If REGEXP-P is non-nil, STRING is taken to be a regular expression.
START is the position to search from; if unspecified, it is
one more than the current position."
  (when (and (stringp string)
             (not (string= string "")))
    (let* ((orig (point))
           (start (or start
                      (if forward
                          (min (point-max) (1+ orig))
                        orig)))
           (isearch-regexp regexp-p)
           (isearch-forward forward)
           (search-func (evil-search-function
                         forward regexp-p evil-search-wrap)))
      ;; no text properties, thank you very much
      (set-text-properties 0 (length string) nil string)
      ;; position to search from
      (goto-char start)
      (condition-case nil
          (funcall search-func string)
        (search-failed
         (goto-char orig)
         (error "\"%s\": %s not found"
                string (if regexp-p "pattern" "string"))))
      (setq isearch-string string)
      (isearch-update-ring string regexp-p)
      ;; handle opening and closing of invisible area
      (funcall isearch-filter-predicate
               (match-beginning 0) (match-end 0))
      ;; always position point at the beginning of the match
      (goto-char (match-beginning 0))
      ;; determine message for echo area
      (cond
       ((and forward (< (point) start))
        (setq string "Search wrapped around BOTTOM of buffer"))
       ((and (not forward) (> (point) start))
        (setq string "Search wrapped around TOP of buffer"))
       (t
        (setq string (evil-search-message string forward))))
      (evil-flash-search-pattern string t))))

(defun evil-search-symbol (forward)
  "Search for symbol near point.
If FORWARD is nil, search backward, otherwise forward."
  (let ((string (car-safe regexp-search-ring))
        (move (if forward 'forward-char 'backward-char))
        (end (if forward 'eobp 'bobp)))
    (setq isearch-forward forward)
    (cond
     ((and (memq last-command
                 '(evil-search-symbol-forward
                   evil-search-symbol-backward))
           (stringp string)
           (not (string= string "")))
      (evil-search string forward evil-search-wrap))
     (t
      (setq string (evil-find-symbol forward))
      (if (null string)
          (error "No symbol under point")
        (setq string (format "\\_<%s\\_>" (regexp-quote string))))
      (evil-search string forward evil-search-wrap)))))

(defun evil-find-symbol (forward)
  "Return symbol near point as a string.
If FORWARD is nil, search backward, otherwise forward.
Returns nil if nothing is found."
  (let ((move (if forward 'forward-char 'backward-char))
        (end (if forward 'eobp 'bobp))
        string)
    (save-excursion
      (setq string (thing-at-point 'symbol))
      ;; if there's nothing under point, go forwards
      ;; (or backwards) to find it
      (while (and (null string) (not (funcall end)))
        (funcall move)
        (setq string (thing-at-point 'symbol)))
      (when (stringp string)
        (set-text-properties 0 (length string) nil string))
      (when (> (length string) 0)
        string))))

(defun evil-search-prompt (forward)
  "Return the search prompt for the given direction."
  (if forward "/" "?"))

(defun evil-search-message (string forward)
  "Prefix STRING with the search prompt."
  (format "%s%s" (evil-search-prompt forward) string))

(defadvice isearch-message-prefix (around evil activate)
  "Use `evil-search-prompt'."
  (if evil-search-prompt
      (setq ad-return-value evil-search-prompt)
    ad-do-it))

(defadvice isearch-delete-char (around evil activate)
  "Exit search if no search string."
  (if (and evil-search-prompt
           (string= isearch-string ""))
      (isearch-exit)
    ad-do-it))

(defadvice isearch-lazy-highlight-search (around evil activate)
  "Never wrap the search in this context."
  (let (evil-search-wrap)
    ad-do-it))



;;; Ex-Searching

;; A pattern.
(defun evil-ex-make-pattern (regex casefold whole-line)
  "Creates a new search pattern.
REGEX is the regular expression to be searched for.  CASEFOLD is
the case-fold property of the search. If WHOLE-LINE is non nil
all occurences of the pattern on a line will be highlighted,
otherwise only the first one."
  (list (evil-ex-regex-without-case regex)
        (evil-ex-regex-case regex casefold)
        whole-line))

(defun evil-ex-pattern-regex (pattern)
  "Returns the regular expression of a search PATTERN."
  (car pattern))

(defun evil-ex-pattern-case-fold (pattern)
  "Returns the case-fold property of a search PATTERN."
  (cadr pattern))

(defun evil-ex-pattern-whole-line (pattern)
  "Returns the whole-line property of a search PATTERN."
  (nth 2 pattern))

(defun evil-ex-regex-without-case (re)
  "Returns the regular expression without all occurrences of \\c and \\C."
  (replace-regexp-in-string
   "\\\\."
   #'(lambda (txt)
       (if (member (aref txt 1) '(?c ?C))
           ""
         txt))
   re t t))

(defun evil-ex-regex-case (re default-case)
  "Returns the case as implied by \\c or \\C in regular expression `re'.
If \\c appears anywhere in the pattern, the pattern is case
insenstive, if \\C appears the pattern is case sensitive. Only
the first occurrence of \\c or \\C is used, all others are
ignored. If neither \\c nor \\C appears in the pattern, the
case specified by `default-case' is used. `default-case' should be either
'sensitive, 'insensitive or 'smart. In the latter case the pattern will be
case-sensitive if and only if it contains an upper-case letter, otherwise it
will be case-insensitive."
  (let ((start 0)
        recase)
    (while (and (not recase)
                (string-match "\\\\." re start))
      (case (aref re (1- (match-end 0)))
        (?c (setq recase 'insensitive))
        (?C (setq recase 'sensitive))
        (t (setq start (match-end 0)))))
    (or recase
        (case default-case
          ((sensitive insensitive) default-case)
          (smart (if (isearch-no-upper-case-p re t) 'insensitive 'sensitive))
          (t nil)))))

(defun evil-ex-make-hl (name &rest args)
  "Creates a new highlight object with name NAME and properties ARGS."
  (unless (symbolp name) (error "Excepted symbol as name of highlight."))
  (let ((face 'evil-ex-lazy-highlight)
        (win (selected-window))
        min max match-hook)
    (while args
      (let ((key (pop args))
            (val (pop args)))
        (cond
         ((eq key :face) (setq face val))
         ((eq key :win)  (setq win val))
         ((eq key :min)  (setq min val))
         ((eq key :max)  (setq max val))
         ((eq key :match-hook) (setq match-hook val))
         (t (error "Unexpected keyword: %s" key)))))
    (when (assoc name evil-ex-active-highlights-alist)
      (evil-ex-delete-hl name))
    (when (null evil-ex-active-highlights-alist)
      (add-hook 'window-scroll-functions #'evil-ex-hl-update-highlights-scroll nil t)
      (add-hook 'window-size-change-functions #'evil-ex-hl-update-highlights-resize nil))
    (push (cons name (vector name nil face win min max match-hook nil))
          evil-ex-active-highlights-alist)))

(defun evil-ex-hl-name (hl)
  "Returns the name of the highlight HL."
  (aref hl 0))

(defun evil-ex-hl-pattern (hl)
  "Returns the pattern of the highlight HL."
  (aref hl 1))

(defun evil-ex-hl-set-pattern (hl pattern)
  "Sets the pattern of the highlight HL to PATTERN."
  (aset hl 1 pattern))

(defun evil-ex-hl-face (hl)
  "Returns the face of the highlight HL."
  (aref hl 2))

(defun evil-ex-hl-window (hl)
  "Returns the window of the highlight HL."
  (aref hl 3))

(defun evil-ex-hl-min (hl)
  "Returns the minimal buffer position of the highlight HL."
  (aref hl 4))

(defun evil-ex-hl-set-min (hl min)
  "Sets the minimal buffer position of the highlight HL to MIN."
  (aset hl 4 min))

(defun evil-ex-hl-max (hl)
  "Returns the maximal buffer position of the highlight HL."
  (aref hl 5))

(defun evil-ex-hl-set-max (hl max)
  "Sets the minimal buffer position of the highlight HL to MAX."
  (aset hl 5 max))

(defun evil-ex-hl-match-hook (hl)
  "Returns the match-hook of the highlight HL."
  (aref hl 6))

(defun evil-ex-hl-overlays (hl)
  "Returns the list of active overlays of the highlight HL."
  (aref hl 7))

(defun evil-ex-hl-set-overlays (hl overlays)
  "Sets the list of active overlays of the highlight HL to OVERLAYS."
  (aset hl 7 overlays))


(defun evil-ex-delete-hl (name)
  "Removes the highlighting object with a certain `name'."
  (let ((hl (cdr-safe (assoc name evil-ex-active-highlights-alist))))
    (when hl
      (mapc #'delete-overlay (evil-ex-hl-overlays hl))
      (setq evil-ex-active-highlights-alist
            (assq-delete-all name evil-ex-active-highlights-alist))
      (evil-ex-hl-update-highlights))
    (when (null evil-ex-active-highlights-alist)
      (remove-hook 'window-scroll-functions #'evil-ex-hl-update-highlights-scroll t)
      (remove-hook 'window-size-change-functions #'evil-ex-hl-update-highlights-resize))))


(defun evil-ex-hl-active-p (name)
  "Returns t iff the highlight with a certain name is active."
  (and (assoc name evil-ex-active-highlights-alist) t))


(defun evil-ex-hl-change (name new-pattern)
  "Sets the regular expression of the highlighting object with
name `name' to `new-regex'."
  (let ((hl (cdr-safe (assoc name evil-ex-active-highlights-alist))))
    (when hl
      (evil-ex-hl-set-pattern hl
                              (if (zerop (length new-pattern))
                                  nil
                                new-pattern))
      (evil-ex-hl-idle-update))))


(defun evil-ex-hl-set-region (name beg end)
  (let ((hl (cdr-safe (assoc name evil-ex-active-highlights-alist))))
    (when hl
      (evil-ex-hl-set-min hl beg)
      (evil-ex-hl-set-max hl end)
      (evil-ex-hl-idle-update))))


(defun evil-ex-hl-update-highlights ()
  "Updates the overlays of all active highlights."
  (dolist (hl (mapcar #'cdr evil-ex-active-highlights-alist))
    (let ((old-ovs (evil-ex-hl-overlays hl))
          new-ovs
          (pattern (evil-ex-hl-pattern hl))
          (face (evil-ex-hl-face hl))
          (match-hook (evil-ex-hl-match-hook hl))
          result)
      (condition-case lossage
          (progn
            (when pattern
              (dolist (win (if (eq evil-ex-interactive-search-highlight 'all-windows)
                               (get-buffer-window-list (current-buffer) nil t)
                             (list (evil-ex-hl-window hl))))
                (let ((begin (max (window-start win)
                                  (or (evil-ex-hl-min hl) (point-min))))
                      (end (min (window-end win)
                                (or (evil-ex-hl-max hl) (point-max))))
                      last-line)
                  (when (< begin end)
                    (save-excursion
                      (goto-char begin)
                      ;; set the overlays for the current highlight, reusing old overlays
                      ;; (if possible)
                      (while (and (evil-ex-search-find-next-pattern pattern)
                                  (< (match-beginning 0) (match-end 0))
                                  (<= (match-end 0) end))
                        (when (or (evil-ex-pattern-whole-line pattern)
                                  (not (equal (line-number-at-pos (match-beginning 0)) last-line)))
                          (setq last-line (line-number-at-pos (match-beginning 0)))
                          (push (if old-ovs
                                    (progn
                                      (move-overlay (car old-ovs)
                                                    (match-beginning 0)
                                                    (match-end 0))
                                      (overlay-put (car old-ovs) 'face face)
                                      (pop old-ovs))
                                  (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
                                    (overlay-put ov 'face face)
                                    (overlay-put ov 'evil-ex-hl (evil-ex-hl-name hl))
                                    (overlay-put ov 'priority 1000)
                                    ov))
                                new-ovs)
                          (when match-hook (funcall match-hook (car new-ovs))))))))))

            (mapc #'delete-overlay old-ovs)
            (evil-ex-hl-set-overlays hl new-ovs)
            (if (or (null pattern) new-ovs)
                (setq result t)
              ;; maybe the match could just not be found somewhere else?
              (save-excursion
                (goto-char (evil-ex-hl-min hl))
                (if (and (evil-ex-search-find-next-pattern pattern)
                         (< (match-end 0) (evil-ex-hl-max hl)))
                    (setq result (format "Match in line %d" (line-number-at-pos (match-beginning 0))))
                  (setq result "No match")))))

        (invalid-regexp
         (setq result (cadr lossage)))

        (search-failed
         (setq result (nth 2 lossage)))

        (error
         (setq result (format "%s" lossage)))))))

(defun evil-ex-search-find-next-pattern (pattern &optional direction)
  "Looks for the next occurrence of pattern in a certain direction."
  (setq direction (or direction 'forward))
  (let ((case-fold-search (eq (evil-ex-pattern-case-fold pattern) 'insensitive)))
    (case direction
      ('forward (re-search-forward (evil-ex-pattern-regex pattern) nil t))
      ('backward (re-search-backward (evil-ex-pattern-regex pattern) nil t))
      (t (error "Unknown search direction: %s" direction)))))


(defun evil-ex-hl-idle-update ()
  "Triggers the timer to update the highlights in the current buffer."
  (when (and evil-ex-interactive-search-highlight
             evil-ex-active-highlights-alist)
    (when evil-ex-hl-update-timer
      (cancel-timer evil-ex-hl-update-timer))
    (setq evil-ex-hl-update-timer
          (run-at-time 0.1 nil
                       #'evil-ex-hl-do-update-highlight
                       (current-buffer)))))


(defun evil-ex-hl-do-update-highlight (&optional buffer)
  "Timer function, updating the highlights."
  (with-current-buffer buffer
    (evil-ex-hl-update-highlights))
  (setq evil-ex-hl-update-timer nil))


(defun evil-ex-hl-update-highlights-scroll (win begin)
  "Update highlights after scrolling in some window."
  (with-current-buffer (window-buffer)
    (evil-ex-hl-idle-update)))


(defun evil-ex-hl-update-highlights-resize (frame)
  "Updates highlights after resizing a window."
  (let ((buffers (delete-dups (mapcar #'window-buffer (window-list frame)))))
    (dolist (buf buffers)
      (with-current-buffer buf
        (evil-ex-hl-idle-update)))))

;; Interactive search.

(defun evil-ex-search-next ()
  "Search for the next occurrence of pattern."
  (let ((retry t))
    (setq isearch-success nil
          isearch-error nil)
    (condition-case lossage
        (progn
          (while retry
            (let ((search-result (evil-ex-find-next)))
              (case search-result
                ((t) (setq isearch-success t
                           isearch-wrapped nil))
                ((nil) (setq isearch-success nil
                             isearch-wrapped nil))
                (t (setq isearch-success t
                         isearch-wrapped t))))
            (setq isearch-success (evil-ex-find-next))
            ;; Clear RETRY unless we matched some invisible text
            ;; and we aren't supposed to do that.
            (when (or (eq search-invisible t)
                      (not isearch-success)
                      (bobp) (eobp)
                      (= (match-beginning 0) (match-end 0))
                      (not (isearch-range-invisible
                            (match-beginning 0) (match-end 0))))
              (setq retry nil)))
          (setq isearch-just-started nil))

      (invalid-regexp
       (setq isearch-error (cadr lossage)))

      (search-failed
       (setq isearch-error (nth 2 lossage)))

      (error
       (setq isearch-error (format "%s" lossage))))

    (cond
     (isearch-success
      (setq isearch-other-end (if (eq evil-ex-search-direction 'forward) (match-beginning 0) (match-end 0))))
     ((not isearch-error)
      (setq isearch-error "No match")))
    (if isearch-wrapped
        (if isearch-error
            (setq isearch-message (concat "Wrapped, " isearch-error))
          (setq isearch-message "Wrapped"))
      (setq isearch-message isearch-error))))


(defun evil-ex-find-next ()
  "Searches the next occurrence w.r.t. actual search data,
possibly wrapping and eob or bob."
  (if (zerop (length (evil-ex-pattern-regex evil-ex-search-pattern)))
      (progn
        (setq evil-ex-search-match-beg nil
              evil-ex-search-match-end nil)
        t)
    (let (wrapped
          result
          (retry t))
      (save-excursion
        (while retry
          (setq retry (not wrapped))
          (cond
           ;; normal search
           ((evil-ex-search-find-next-pattern evil-ex-search-pattern
                                              evil-ex-search-direction)
            (setq evil-ex-search-match-beg (match-beginning 0)
                  evil-ex-search-match-end (match-end 0)
                  result (if wrapped 1 t)
                  retry nil))

           ;; wrap and eob and bob
           ((not wrapped)
            (goto-char (case evil-ex-search-direction
                         ('forward (point-min))
                         ('backward (point-max))))
            (setq wrapped t))

           ;; already wrapped, search failed
           (t
            (setq evil-ex-search-match-beg nil evil-ex-search-match-end nil
                  result nil
                  retry nil))))
        result))))

(defun evil-ex-search-update ()
  "Updates the highlighting and the info-message for the actual search pattern."
  (evil-ex-message isearch-message)
  (with-current-buffer evil-ex-current-buffer
    (when evil-ex-search-interactive
      (when isearch-success
        (if (null evil-ex-search-match-beg)
            (when evil-ex-search-overlay
              (delete-overlay evil-ex-search-overlay)
              (setq evil-ex-search-overlay nil))
          (goto-char evil-ex-search-match-beg)
          (if evil-ex-search-overlay
              (move-overlay evil-ex-search-overlay
                            evil-ex-search-match-beg
                            evil-ex-search-match-end)
            (setq evil-ex-search-overlay (make-overlay evil-ex-search-match-beg evil-ex-search-match-end))
            (overlay-put evil-ex-search-overlay 'priority 1001)
            (overlay-put evil-ex-search-overlay 'face 'evil-ex-search))))
      (when evil-ex-search-highlight-all
        (evil-ex-hl-change 'evil-ex-search (and isearch-success evil-ex-search-pattern))))))

(defun evil-ex-search-start-session ()
  "Called to initialize ex-mode for interactive search."
  (remove-hook 'minibuffer-setup-hook #'evil-ex-search-start-session)
  (add-hook 'after-change-functions #'evil-ex-search-update-pattern nil t)
  (add-hook 'minibuffer-exit-hook #'evil-ex-search-stop-session)
  (when (and evil-ex-search-interactive evil-ex-search-highlight-all)
    (with-current-buffer evil-ex-current-buffer
      (evil-ex-make-hl 'evil-ex-search))))

(defun evil-ex-search-stop-session ()
  "Stops interactive search."
  (with-current-buffer evil-ex-current-buffer
    ;; TODO: this is a bad fix to remove duplicates.
    ;;       The duplicates exist because isearch-range-invisible
    ;;       may add a single overlay multiple times if we are
    ;;       in an unlucky situation of overlapping overlays. This
    ;;       happens in our case because of the overlays that are
    ;;       used for (lazy) highlighting. Perhaps it would be better
    ;;       to disable those overlays temporarily before calling
    ;;       isearch-range-invisible.
    ;;  the following code is equivalent to
    (setq isearch-opened-overlays (delete-dups isearch-opened-overlays))
    (isearch-clean-overlays))
  (remove-hook 'minibuffer-exit-hook #'evil-ex-search-stop-session)
  (remove-hook 'after-change-functions #'evil-ex-search-update-pattern t)
  (when evil-ex-search-overlay
    (delete-overlay evil-ex-search-overlay)
    (setq evil-ex-search-overlay nil)))

(defun evil-ex-search-update-pattern (beg end range)
  "Called to update the current search pattern."
  (setq evil-ex-search-pattern
        (evil-ex-make-pattern (minibuffer-contents)
                              evil-ex-search-case
                              t))
  (with-current-buffer evil-ex-current-buffer
    (with-selected-window (minibuffer-selected-window)
        (goto-char evil-ex-search-start-point)
        (save-excursion
          (dotimes (i (or evil-ex-search-count 1))
            (if (eq evil-ex-search-direction 'backward)
                (backward-char)
              (forward-char))
            (evil-ex-search-next)
            (when evil-ex-search-match-beg
              (goto-char evil-ex-search-match-beg))))))
  (evil-ex-search-update))


(defun evil-ex-search-exit ()
  "Exits interactive search, lazy highlighting keeps active."
  (interactive)
  (evil-ex-search-stop-session)
  (exit-minibuffer))

(defun evil-ex-search-abort ()
  "Aborts interactive search, disables lazy highlighting."
  (interactive)
  (evil-ex-search-stop-session)
  (evil-ex-delete-hl 'evil-ex-search)
  (abort-recursive-edit))

(defun evil-ex-start-search (direction count)
  "Starts a new search in a certain direction."
  ;; store buffer and window where the search started
  (let ((evil-ex-current-buffer (current-buffer)))
    (setq evil-ex-search-count count
          evil-ex-search-direction direction
          evil-ex-search-start-point (point)
          evil-ex-search-match-beg nil
          evil-ex-search-match-end nil)

    (condition-case err
        (progn
          ;; ensure minibuffer is initialized accordingly
          (add-hook 'minibuffer-setup-hook #'evil-ex-search-start-session)
          ;; read the search string
          (let ((minibuffer-local-map evil-ex-keymap))
            (when (read-string (case evil-ex-search-direction
                                 ('forward "/")
                                 ('backward "?"))
                               nil 'evil-ex-search-history)
              (goto-char evil-ex-search-start-point)
              (if evil-ex-search-match-beg
                  (goto-char evil-ex-search-match-beg)
                (evil-ex-find-next)))))
      (quit
       (evil-ex-search-stop-session)
       (evil-ex-delete-hl 'evil-ex-search)
       (goto-char evil-ex-search-start-point)
       (signal (car err) (cdr err))))))

(defun evil-ex-nohighlight ()
  "Disables the active search highlightings."
  (interactive)
  (evil-ex-delete-hl 'evil-ex-search))

(provide 'evil-search)

;;; evil-search.el ends here
