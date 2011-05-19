;;;; Search

(require 'evil-vars)
(require 'evil-common)




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
  (let (evil-search-wrap)
    ad-do-it))

;; if `evil-search-wrap' is t, we want the search to wrap
(defun evil-search-fun-function ()
  "Return a wrapping search function.
Based on `isearch-regexp' and `isearch-forward'."
  `(lambda (regexp &optional bound noerror count)
     (let ((point (point))
           (search-fun (if isearch-regexp
                           (if isearch-forward
                               're-search-forward
                             're-search-backward)
                         (if isearch-forward
                             'search-forward
                           'search-backward)))
           result)
       (setq result (funcall search-fun regexp bound t count))
       (when (and (not result) evil-search-wrap)
         (goto-char (if isearch-forward (point-min) (point-max)))
         (setq result (funcall search-fun regexp bound t count))
         (unless result
           (goto-char point)))
       result)))

(defun evil-search-backward (&optional forward)
  "Search incrementally for user-entered text."
  (interactive)
  (evil-search-forward (not forward)))

(defun evil-search-forward (&optional backward)
  "Search incrementally for user-entered text."
  (interactive)
  (let ((evil-search-prompt "/")
        (isearch-forward (not backward))
        (isearch-search-fun-function 'evil-search-fun-function)
        (point (point))
        search-nonincremental-instead)
    (evil-save-echo-area
      (isearch-forward isearch-regexp)
      (when (and (eq point (point))
                 (not (string= isearch-string "")))
        (isearch-repeat-forward)
        (isearch-exit))
      (when isearch-other-end
        (goto-char isearch-other-end))
      (unless (string= isearch-string "")
        (evil-flash-search-pattern)))))

(put 'evil-search-forward 'function-documentation
     (format "Search forward for user-entered text.
Searches for regular expression if `isearch-regexp' is t.

%s" (if (and (fboundp 'isearch-forward)
             (documentation 'isearch-forward))
        (format "Below is the documentation string for `isearch-forward',
which lists available keys:

%s" (documentation 'isearch-forward)))))

(defun evil-flash-search-pattern (&optional all)
  "Flash search matches for duration of `evil-flash-delay'."
  (let ((lazy-highlight-initial-delay 0)
        (isearch-search-fun-function 'evil-search-fun-function)
        (isearch-case-fold-search case-fold-search)
        (disable (lambda (&optional arg) (evil-flash-hook t))))
    (when evil-flash-timer
      (cancel-timer evil-flash-timer))
    (isearch-highlight (match-beginning 0) (match-end 0))
    (when all
      (setq isearch-lazy-highlight-wrapped nil
            isearch-lazy-highlight-start (point)
            isearch-lazy-highlight-end (point))
      (isearch-lazy-highlight-new-loop)
      (unless isearch-lazy-highlight-overlays
        (isearch-lazy-highlight-update)))
    (add-hook 'pre-command-hook 'evil-flash-hook)
    (setq evil-flash-timer
          (run-at-time evil-flash-delay nil disable))))

(defun evil-flash-hook (&optional force)
  "Disable hightlighting if `this-command' is not search.
Disable anyway if FORCE is t."
  (when (or force
            ;; to avoid flicker, don't disable highlighting if the
            ;; next command is also a search command
            (not (memq this-command
                       '(evil-exec-mapped-kbd-macro
                         evil-search
                         evil-search-backward
                         isearch-forward
                         evil-search-next
                         evil-search-Next
                         evil-search-backward
                         isearch-forward
                         evil-search-backward-for-symbol-at-point
                         evil-search-forward-for-symbol-at-point))))
    (isearch-dehighlight)
    (setq isearch-lazy-highlight-last-string nil)
    (lazy-highlight-cleanup t)
    (when evil-flash-timer
      (cancel-timer evil-flash-timer)))
  (remove-hook 'pre-command-hook 'evil-flash-hook))

(provide 'evil-search)

;;; evil-search.el ends here
