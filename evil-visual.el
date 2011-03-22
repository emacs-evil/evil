;;;; Visual state

;; Visual selections are implemented in terms of types, and are
;; compatible with the Emacs region. This is achieved by "translating"
;; the region to the selected text right before a command is executed.
;; If the command is a motion, the translation is postponed until a
;; non-motion command is invoked.
;;
;; Visual state activates the region, enabling Transient Mark mode if
;; not already enabled. This is only temporay: if Transient Mark mode
;; was disabled before entering Visual state, it is disabled when
;; exiting Visual state. This allows Visual state to harness the
;; "transient" behavior of many commands without overriding the user's
;; preferences in other states.

(require 'evil-vars)
(require 'evil-common)
(require 'evil-states)
(require 'evil-types)

(evil-define-state visual
  "Visual state."
  :tag " <V> "
  :enable (motion normal)
  (cond
   ((evil-visual-state-p)
    (evil-transient-save)
    (evil-visual-select (point) (point) evil-visual-char
                        (evil-called-interactively-p))
    (add-hook 'pre-command-hook 'evil-visual-pre-command nil t)
    (add-hook 'post-command-hook 'evil-visual-post-command nil t)
    (setq evil-visual-region-expanded nil))
   (t
    (remove-hook 'pre-command-hook 'evil-visual-pre-command t)
    (remove-hook 'post-command-hook 'evil-visual-post-command t)
    (evil-visual-highlight -1)
    (evil-active-region -1)
    (evil-transient-restore))))

(defmacro evil-define-visual-selection (selection doc &rest body)
  "Define a Visual selection SELECTION.
Creates a command evil-visual-SELECTION for enabling the selection.
DOC is the function's documentation string. The following keywords
may be specified in BODY:

:message STRING         Status message when enabling the selection.
:type TYPE              Type to use (defaults to SELECTION).

Following the keywords is optional code which is executed each time
the selection is enabled."
  (declare (indent defun)
           (debug (&define name stringp
                           [&rest keywordp sexp]
                           def-body)))
  (let* ((name (intern (format "evil-visual-%s" selection)))
         (message (intern (format "%s-message" name)))
         (type selection)
         string keyword)
    ;; collect keywords
    (while (keywordp (car-safe body))
      (setq keyword (pop body))
      (cond
       ((eq keyword :message)
        (setq string (pop body)))
       ((eq keyword :type)
        (setq type (pop body)))
       (t
        (pop body))))
    ;; macro expansion
    `(progn
       (add-to-list 'evil-visual-alist (cons ',selection ',name))
       (defvar ,name ',type ,doc)
       (defvar ,message ,string ,doc)
       (defun ,name (&optional mark point message)
         ,@(when doc `(,doc))
         (interactive (list nil nil t))
         (if (and (eq (evil-visual-type) ,name)
                  message)
             (evil-normal-state)
           (setq point (or point (point))
                 mark  (or mark
                           (when (evil-visual-state-p)
                             (mark t))
                           point))
           (unless (evil-visual-state-p)
             (evil-visual-state))
           (evil-active-region 1)
           (evil-move-mark mark)
           (goto-char point)
           (evil-visual-refresh ,name mark point)
           (unless (stringp message)
             (setq message (and message ,message)))
           (when message
             (evil-echo message))
           ,@body))
       ',selection)))

(evil-define-visual-selection char
  "Characterwise selection."
  :type inclusive
  :message "-- VISUAL --")

(evil-define-visual-selection line
  "Linewise selection."
  :message "-- VISUAL LINE --")

(evil-define-visual-selection block
  "Blockwise selection."
  :message "-- VISUAL BLOCK --"
  (evil-transient-mark -1)
  (overlay-put evil-visual-overlay 'corner nil)
  (overlay-put evil-visual-overlay 'corner (evil-visual-block-corner)))

(defun evil-visual-pre-command ()
  "Run before each command in Visual state.
Unless `this-command' is a motion, expand the region
to the selection."
  (when (evil-visual-state-p)
    (setq evil-this-type (evil-visual-type))
    (unless (or (evil-motion-p this-command)
                (rassq this-command evil-visual-alist)
                (rassq this-command (evil-state-property nil :mode))
                (memq this-command '(evil-visual-restore)))
      (evil-visual-expand-region
       ;; don't include final newline in linewise selection
       ;; unless the command has real need of it
       (and (eq (evil-visual-type) 'line)
            (not (evil-operator-p this-command)))))))

(defun evil-visual-post-command ()
  "Run after each command in Visual state.
If `this-command' was a motion, refresh the selection;
otherwise exit Visual state."
  (when (evil-visual-state-p)
    (if (or quit-flag
            (eq this-command 'keyboard-quit)
            evil-visual-region-expanded)
        (evil-normal-state)
      (evil-visual-refresh))))

(defun evil-visual-select (&optional mark point type message)
  "Create a Visual selection with MARK, POINT and TYPE.
If MESSAGE is a string, echo MESSAGE; if MESSAGE is non-nil,
echo a pre-defined message."
  (let* ((type (or type (evil-visual-type) evil-visual-char))
         (func (or (cdr (assq type evil-visual-alist))
                   (cdr (assq type (evil-visual-alist))))))
    ;; if TYPE has a selection function, use that;
    ;; otherwise enable the selection in a generic way
    (if func
        (funcall func mark point message)
      (unless (evil-visual-state-p)
        (evil-visual-state 1))
      (evil-active-region 1)
      (evil-move-mark mark)
      (goto-char point)
      (evil-visual-refresh type mark point)
      (when (stringp message)
        (evil-echo message)))))

(defun evil-visual-expand-region (&optional no-trailing-newline)
  "Expand the region to the Visual selection.
If NO-TRAILING-NEWLINE is t and the selection ends with a newline,
exclude that newline from the region."
  (let ((beg (evil-visual-beginning))
        (end (evil-visual-end)))
    (when no-trailing-newline
      (save-excursion
        (goto-char end)
        (when (and (bolp) (not (bobp)))
          (setq end (max beg (1- (point)))))))
    (setq evil-visual-region-expanded t)
    (evil-set-region beg end)))

(defun evil-visual-refresh (&optional type mark point)
  "Refresh `evil-visual-overlay'."
  (let* ((point (or point (point)))
         (mark  (or mark (mark t) point))
         (dir   (if (< point mark) -1 1))
         (type  (or type (evil-visual-type) evil-visual-char)))
    (if (null evil-visual-overlay)
        (setq evil-visual-overlay (make-overlay mark point))
      (evil-contract-overlay evil-visual-overlay)
      (move-overlay evil-visual-overlay mark point))
    (overlay-put evil-visual-overlay 'direction dir)
    (evil-set-type evil-visual-overlay type)
    (evil-expand-overlay evil-visual-overlay)
    (evil-visual-highlight)))

(defun evil-visual-highlight (&optional arg)
  "Highlight Visual selection, depending on the Visual type.
With negative ARG, disable highlighting."
  (cond
   ((and (numberp arg) (< arg 1))
    (overlay-put evil-visual-overlay 'face nil)
    (mapc 'delete-overlay evil-visual-block-overlays)
    (setq evil-visual-block-overlays nil))
   ((eq (evil-visual-type) 'block)
    (overlay-put evil-visual-overlay 'face nil)
    (evil-visual-highlight-block
     (evil-visual-beginning)
     (evil-visual-end)))
   (t
    (evil-visual-highlight -1)
    (overlay-put evil-visual-overlay 'face 'region)
    (overlay-put evil-visual-overlay 'priority 99))))

(defun evil-visual-highlight-block (beg end &optional overlays)
  "Highlight rectangular region from BEG to END.
Do this by putting an overlay on each line within the rectangle.
Each overlay extends across all the columns of the rectangle.
Reuse overlays where possible to prevent flicker."
  (let* ((point (point))
         (mark (or (mark t) point))
         (overlays (or overlays 'evil-visual-block-overlays))
         (old (symbol-value overlays))
         beg-col end-col new nlines overlay window-beg window-end)
    ;; calculate the rectangular region represented by BEG and END,
    ;; but put BEG in the upper-left corner and END in the lower-right
    ;; if not already there
    (save-excursion
      (setq beg-col (save-excursion (goto-char beg)
                                    (current-column))
            end-col (save-excursion (goto-char end)
                                    (current-column)))
      (when (>= beg-col end-col)
        (if (= beg-col end-col)
            (setq end-col (1+ end-col))
          (evil-sort beg-col end-col))
        (setq beg (save-excursion (goto-char beg)
                                  (evil-move-to-column beg-col)
                                  (point))
              end (save-excursion (goto-char end)
                                  (evil-move-to-column end-col 1)
                                  (point))))
      ;; force a redisplay so we can do reliable window
      ;; BEG/END calculations
      (sit-for 0)
      (setq window-beg (max (window-start) beg)
            window-end (min (window-end) (1+ end))
            nlines (count-lines window-beg
                                (min window-end (point-max))))
      ;; iterate over those lines of the rectangle which are
      ;; visible in the currently selected window
      (goto-char window-beg)
      (dotimes (i nlines)
        (let (before after row-beg row-end)
          ;; beginning of row
          (evil-move-to-column beg-col)
          (when (< (current-column) beg-col)
            ;; prepend overlay with virtual spaces if unable to
            ;; move directly to the first column
            (setq before
                  (propertize
                   (make-string
                    (- beg-col (current-column)) ?\ )
                   'face
                   (or (get-text-property (1- (point)) 'face)
                       'default))))
          (setq row-beg (point))
          ;; end of row
          (evil-move-to-column end-col)
          (when (< (current-column) end-col)
            ;; append overlay with virtual spaces if unable to
            ;; move directly to the last column
            (setq after
                  (propertize
                   (make-string
                    (if (= (point) row-beg)
                        (- end-col beg-col)
                      (- end-col (current-column)))
                    ?\ ) 'face 'region))
            ;; place cursor on one of the virtual spaces
            (if (= point row-beg)
                (put-text-property
                 0 (min (length after) 1)
                 'cursor t after)
              (put-text-property
               (max 0 (1- (length after))) (length after)
               'cursor t after)))
          (setq row-end (min (point) (line-end-position)))
          ;; trim old leading overlays
          (while (and old
                      (setq overlay (car old))
                      (< (overlay-start overlay) row-beg)
                      (/= (overlay-end overlay) row-end))
            (delete-overlay overlay)
            (setq old (cdr old)))
          ;; reuse an overlay if possible, otherwise create one
          (cond
           ((and old (setq overlay (car old))
                 (or (= (overlay-start overlay) row-beg)
                     (= (overlay-end overlay) row-end)))
            (move-overlay overlay row-beg row-end)
            (overlay-put overlay 'before-string before)
            (overlay-put overlay 'after-string after)
            (setq new (cons overlay new)
                  old (cdr old)))
           (t
            (setq overlay (make-overlay row-beg row-end))
            (overlay-put overlay 'before-string before)
            (overlay-put overlay 'after-string after)
            (overlay-put overlay 'face 'region)
            (overlay-put overlay 'priority 99)
            (setq new (cons overlay new)))))
        (forward-line 1))
      ;; trim old trailing overlays
      (mapc 'delete-overlay old)
      (set overlays (nreverse new)))))

(defun evil-visual-beginning (&optional force)
  "Return beginning of Visual selection.
FORCE returns the previous beginning if not in Visual state."
  (when (or force (evil-visual-state-p))
    (and (overlayp evil-visual-overlay)
         (overlay-start evil-visual-overlay))))

(defun evil-visual-end (&optional force)
  "Return end of Visual selection.
FORCE returns the previous end if not in Visual state."
  (when (or force (evil-visual-state-p))
    (and (overlayp evil-visual-overlay)
         (overlay-end evil-visual-overlay))))

(defun evil-visual-type (&optional force)
  "Return current Visual type, nil if not in Visual state.
FORCE returns the previous Visual type if not in Visual state."
  (when (or force (evil-visual-state-p))
    (and (overlayp evil-visual-overlay)
         (overlay-get evil-visual-overlay 'type))))

;; recognizes user changes, e.g., customizing
;; `evil-visual-char' to `exclusive'
(defun evil-visual-alist ()
  "Return an association list from types to selection functions."
  (mapcar (lambda (e)
            (cons (symbol-value (cdr-safe e)) (cdr-safe e)))
          evil-visual-alist))

(defun evil-visual-restore ()
  "Restore previous selection."
  (interactive)
  (let* ((point (point))
         (mark (or (mark t) point))
         (type (evil-visual-type t))
         dir)
    (unless (evil-visual-state-p)
      (when evil-visual-overlay
        (evil-contract-overlay evil-visual-overlay)
        (setq mark (evil-visual-beginning t)
              point (evil-visual-end t)
              dir (overlay-get evil-visual-overlay 'direction))
        (when (< dir 0)
          (evil-swap mark point)))
      (evil-visual-select mark point type t))))

(defun evil-visual-exchange-corners ()
  "Rearrange corners in Visual Block mode.

        M---+           +---M
        |   |    <=>    |   |
        +---P           P---+

For example, if mark is in the upper left corner and point
in the lower right, this function puts mark in the upper right
corner and point in the lower left."
  (interactive)
  (cond
   ((eq (evil-visual-type) 'block)
    (let* ((point (point))
           (mark (or (mark t) point))
           (point-col (current-column))
           (mark-col (save-excursion
                       (goto-char mark)
                       (current-column))))
      (evil-move-mark (save-excursion
                        (goto-char mark)
                        (evil-move-to-column point-col)
                        (point)))
      (evil-move-to-column mark-col)))
   (t
    (exchange-point-and-mark))))

(defun evil-visual-block-corner (&optional point mark corner)
  "Block corner corresponding to POINT, with MARK in opposite corner.
Depending on POINT and MARK, the return value is `upper-left',
`upper-right', `lower-left' or `lower-right':

        upper-left +---+ upper-right
                   |   |
        lower-left +---+ lower-right

One-column or one-row blocks are ambiguous. In such cases,
the horizontal or vertical component of CORNER is used.
CORNER defaults to `upper-left'."
  (let* ((point (or point (point)))
         (mark (or mark (mark t)))
         (corner (symbol-name (or corner
                                  (overlay-get evil-visual-overlay
                                               'corner))))
         (point-col (save-excursion
                      (goto-char point)
                      (current-column)))
         (mark-col (save-excursion
                     (goto-char mark)
                     (current-column)))
         (horizontal (or (and (string-match "left\\|right" corner)
                              (match-string 0 corner))
                         "left"))
         (vertical (or (and (string-match "upper\\|lower" corner)
                            (match-string 0 corner))
                       "upper")))
    (cond
     ((< point-col mark-col)
      (setq horizontal "left"))
     ((> point-col mark-col)
      (setq horizontal "right")))
    (cond
     ((< point mark)
      (setq vertical "upper"))
     ((> point mark)
      (setq vertical "lower")))
    (intern (format "%s-%s" vertical horizontal))))

(defun evil-visual-block-rotate (corner &optional beg end)
  "In Visual Block selection, put point in CORNER.
Corner may be one of `upper-left', `upper-right', `lower-left'
and `lower-right':

        upper-left +---+ upper-right
                   |   |
        lower-left +---+ lower-right

When called interactively, the selection is rotated blockwise."
  (interactive
   (let ((corners '(upper-left upper-right lower-right lower-left)))
     (list (or (cadr (memq (evil-visual-block-corner) corners))
               'upper-left))))
  (let* ((point (point))
         (mark (or (mark t) point))
         (beg (or beg (point)))
         (end (or end (mark t) beg))
         (beg-col (save-excursion
                    (goto-char beg)
                    (current-column)))
         (end-col (save-excursion
                    (goto-char end)
                    (current-column))))
    (evil-sort beg end)
    (evil-sort beg-col end-col)
    (unless (memq corner '(upper-left lower-right))
      (evil-swap beg-col end-col))
    (setq point (save-excursion
                  (goto-char beg)
                  (move-to-column beg-col)
                  (point))
          mark  (save-excursion
                  (goto-char end)
                  (move-to-column end-col)
                  (point)))
    (unless (memq corner '(upper-left upper-right))
      (evil-swap mark point))
    (evil-move-mark mark)
    (goto-char point)
    (overlay-put evil-visual-overlay 'corner corner)))

(provide 'evil-visual)

;;; evil-visual.el ends here
