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

(require 'evil-types)
(require 'evil-core)

(defmacro evil-define-visual-selection (selection doc &rest body)
  "Define a Visual selection SELECTION.
Creates a command evil-visual-SELECTION for enabling the selection.
DOC is the function's documentation string. The following keywords
may be specified in BODY:

:message STRING         Status message when enabling the selection.
:type TYPE              Type to use (defaults to SELECTION).

Following the keywords is optional code which is executed each time
the selection is enabled.

\(fn SELECTION DOC [[KEY VAL]...] BODY...)"
  (declare (indent defun)
           (debug (&define name stringp
                           [&rest keywordp sexp]
                           def-body)))
  (let* ((name (intern (format "evil-visual-%s" selection)))
         (message (intern (format "%s-message" name)))
         (type selection)
         arg key string)
    ;; collect keywords
    (while (keywordp (car-safe body))
      (setq key (pop body)
            arg (pop body))
      (cond
       ((eq key :message)
        (setq string arg))
       ((eq key :type)
        (setq type arg))))
    ;; macro expansion
    `(progn
       (add-to-list 'evil-visual-alist (cons ',selection ',name))
       (defvar ,name ',type ,doc)
       (defvar ,message ,string ,doc)
       (evil-define-command ,name (&optional mark point type message)
         ,@(when doc `(,doc))
         :keep-visual t
         (interactive (list nil nil nil t))
         (let ((type (or type ,name)))
           (if (and (evil-called-interactively-p)
                    (evil-visual-state-p)
                    (eq evil-visual-type type))
               (evil-change-to-previous-state)
             (setq evil-visual-selection ',selection)
             (evil-visual-make-region mark point type message)
             ,@body)
           ',selection)))))

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
  ;; refresh the :corner property
  (setq evil-visual-properties
        (plist-put evil-visual-properties :corner
                   (evil-visual-block-corner 'upper-left))))

(evil-define-state visual
  "Visual state."
  :tag " <V> "
  :enable (motion normal)
  :message 'evil-visual-message
  (cond
   ((evil-visual-state-p)
    (evil-transient-save)
    (cond
     ((region-active-p)
      (if (< (evil-visual-direction) 0)
          (evil-visual-select (region-beginning) (region-end)
                              evil-visual-char
                              (evil-visual-direction))
        (evil-visual-make-region (mark t) (point)
                                 evil-visual-char))
      (evil-visual-highlight))
     (t
      (evil-visual-make-region (point) (point) evil-visual-char)))
    (add-hook 'pre-command-hook 'evil-visual-pre-command nil t)
    (add-hook 'post-command-hook 'evil-visual-post-command nil t)
    (add-hook 'deactivate-mark-hook 'evil-visual-deactivate-hook nil t))
   (t
    ;; Postpone deactivation of region if next state is Insert.
    ;; This gives certain insertion commands (auto-pairing characters,
    ;; for example) an opportunity to access the region.
    (if (and (eq evil-next-state 'insert)
             (eq evil-visual-type evil-visual-char))
        (add-hook 'evil-normal-state-entry-hook
                  'evil-visual-deactivate-hook nil t)
      (evil-visual-deactivate-hook))
    (setq evil-visual-region-expanded nil)
    (setq evil-visual-selection nil)
    (remove-hook 'pre-command-hook 'evil-visual-pre-command t)
    (remove-hook 'post-command-hook 'evil-visual-post-command t)
    (remove-hook 'deactivate-mark-hook 'evil-visual-deactivate-hook t)
    (evil-visual-highlight -1))))

(defun evil-visual-pre-command ()
  "Run before each command in Visual state.
Unless `this-command' is a motion, expand the region
to the selection."
  (when (evil-visual-state-p)
    (unless (evil-get-command-property
             this-command :keep-visual)
      (evil-visual-expand-region
       ;; exclude final newline from linewise selection
       ;; unless the command has real need of it
       (and (eq evil-visual-type 'line)
            (not (evil-get-command-property
                  this-command :include-newline)))))))

(defun evil-visual-post-command ()
  "Run after each command in Visual state.
If `this-command' was a motion, refresh the selection;
otherwise exit Visual state."
  (when (evil-visual-state-p)
    (cond
     ((or quit-flag
          (eq this-command 'keyboard-quit)
          ;; Is `mark-active' nil for an unexpanded region?
          (and (not evil-visual-region-expanded)
               (not (region-active-p))
               (not (eq evil-visual-type evil-visual-block))))
      (evil-visual-contract-region)
      (evil-change-to-previous-state))
     (evil-visual-region-expanded
      (evil-visual-contract-region)
      (evil-visual-highlight))
     (t
      (evil-visual-refresh)
      (evil-visual-highlight)))))

(defun evil-visual-deactivate-hook ()
  "Deactivate the region and restore Transient Mark mode."
  (remove-hook 'deactivate-mark-hook
               'evil-visual-deactivate-hook t)
  (remove-hook 'evil-normal-state-entry-hook
               'evil-visual-deactivate-hook t)
  (cond
   ((and (evil-visual-state-p)
         this-command
         (not (evil-get-command-property
               this-command :keep-visual)))
    (evil-change-to-previous-state)
    (evil-active-region -1)
    (evil-transient-restore))
   ((not (evil-visual-state-p))
    (evil-active-region -1)
    (evil-transient-restore))))

(defun evil-visual-message (&optional selection)
  "Create an echo area message for SELECTION.
SELECTION is a kind of selection as defined by
`evil-define-visual-selection', such as `char', `line'
or `block'."
  (let (message)
    (setq selection (or selection evil-visual-selection))
    (when selection
      (setq message
            (symbol-value (intern (format "evil-visual-%s-message"
                                          selection))))
      (cond
       ((functionp message)
        (funcall message))
       ((stringp message)
        (evil-echo message))))))

(defun evil-visual-select (beg end &optional type dir)
  "Create a Visual selection of type TYPE from BEG to END.
Point and mark are positioned so that the resulting selection
has the specified boundaries. If DIR is negative, point precedes mark,
otherwise it succedes it. To specify point and mark directly,
use `evil-visual-make-selection'."
  (let* ((type (or (evil-visual-selection-type type)
                   evil-visual-char))
         (dir (or dir 1))
         (range (evil-contract beg end type))
         (beg (evil-range-beginning range))
         (end (evil-range-end range))
         (type (evil-type range type)))
    (when (< dir 0)
      (evil-swap beg end))
    (evil-visual-make-selection beg end type)))

(defun evil-visual-make-selection (mark point &optional type)
  "Create a Visual selection with point at POINT and mark at MARK.
The boundaries of the selection are inferred from these
and the current TYPE. To specify the boundaries and infer
mark and point, use `evil-visual-select' instead."
  (let* ((mark (evil-normalize-position mark))
         (point (evil-normalize-position point))
         (oldtype (when (evil-visual-state-p)
                    evil-visual-type))
         (type (or type oldtype evil-visual-char))
         (state evil-state))
    (unless (evil-visual-state-p)
      (evil-visual-state))
    ;; if there exists a specific selection function for TYPE,
    ;; use that, otherwise use `evil-visual-make-region'
    (funcall (evil-visual-selection-function type)
             mark point type
             (or (not (evil-visual-state-p state))
                 (not (eq type oldtype))))))

;; the generic selection function, on which all other
;; selections are based
(defun evil-visual-make-region (mark point &optional type message)
  "Create an active region from MARK to POINT.
If TYPE is given, also set the Visual type.
If MESSAGE is given, display it in the echo area."
  (interactive)
  (let* ((point (evil-normalize-position
                 (or point (point))))
         (mark (evil-normalize-position
                (or mark
                    (when (or (evil-visual-state-p)
                              (region-active-p))
                      (mark t))
                    point))))
    (unless (evil-visual-state-p)
      (evil-visual-state))
    (evil-active-region 1)
    (setq evil-visual-region-expanded nil)
    (evil-visual-refresh type mark point)
    (cond
     ((null evil-echo-state))
     ((stringp message)
      (evil-echo message))
     (message
      (cond
       ((stringp evil-visual-state-message)
        (evil-echo evil-visual-state-message))
       ((functionp evil-visual-state-message)
        (funcall evil-visual-state-message)))))))

(defun evil-visual-expand-region (&optional no-trailing-newline)
  "Expand the region to the Visual selection.
If NO-TRAILING-NEWLINE is t and the selection ends with a newline,
exclude that newline from the region."
  (when (and (evil-visual-state-p)
             (not evil-visual-region-expanded))
    (let ((mark evil-visual-beginning)
          (point evil-visual-end))
      (when (< evil-visual-direction 0)
        (evil-swap mark point))
      (setq evil-visual-region-expanded t)
      (evil-visual-refresh nil mark point)
      (when (and no-trailing-newline
                 (save-excursion
                   (goto-char evil-visual-end)
                   (and (bolp) (not (bobp)))))
        (if (< evil-visual-direction 0)
            (evil-move-mark (max point (1- (mark))))
          (goto-char (max mark (1- (point)))))))))

(defun evil-visual-contract-region ()
  "The inverse of `evil-visual-expand-region'.
Create a Visual selection that expands to the current region."
  (evil-visual-refresh)
  (setq evil-visual-region-expanded nil)
  (evil-visual-refresh nil evil-visual-mark evil-visual-point))

(defun evil-visual-refresh (&optional type mark point &rest properties)
  "Refresh point, mark and Visual variables.
Refreshes `evil-visual-beginning', `evil-visual-end',
`evil-visual-mark', `evil-visual-point', `evil-visual-type',
`evil-visual-direction' and `evil-visual-properties'."
  (let* ((point (or point (point)))
         (mark (or mark (mark t) point))
         (dir (evil-visual-direction))
         (type (or type evil-visual-type evil-visual-char))
         range)
    (evil-move-mark mark)
    (goto-char point)
    (setq evil-visual-beginning
          (or evil-visual-beginning
              (let ((marker (make-marker)))
                (move-marker marker (min point mark))))
          evil-visual-end
          (or evil-visual-end
              (let ((marker (make-marker)))
                (set-marker-insertion-type marker t)
                (move-marker marker (max point mark))))
          evil-visual-mark
          (or evil-visual-mark
              (let ((marker (make-marker)))
                (move-marker marker mark)))
          evil-visual-point
          (or evil-visual-point
              (let ((marker (make-marker)))
                (move-marker marker point))))
    (setq evil-visual-properties
          (evil-concat-plists evil-visual-properties properties))
    (cond
     (evil-visual-region-expanded
      (move-marker evil-visual-beginning (min point mark))
      (move-marker evil-visual-end (max point mark))
      ;; if the type is one-to-one, we can safely refresh
      ;; the unexpanded positions as well
      (when (evil-type-property type :one-to-one)
        (setq range (apply #'evil-contract point mark type
                           evil-visual-properties)
              mark (evil-range-beginning range)
              point (evil-range-end range))
        (when (< dir 0)
          (evil-swap mark point))
        (move-marker evil-visual-mark mark)
        (move-marker evil-visual-point point)))
     (t
      (setq range (apply #'evil-expand point mark type
                         evil-visual-properties))
      (move-marker evil-visual-beginning (evil-range-beginning range))
      (move-marker evil-visual-end (evil-range-end range))
      (move-marker evil-visual-mark mark)
      (move-marker evil-visual-point point)))
    (setq evil-visual-direction dir
          evil-visual-type type
          evil-this-type type)))

(defun evil-visual-highlight (&optional arg)
  "Highlight Visual selection, depending on the Visual type.
With negative ARG, disable highlighting."
  (cond
   ((and (numberp arg) (< arg 1))
    (when evil-visual-overlay
      (delete-overlay evil-visual-overlay)
      (setq evil-visual-overlay nil))
    (when evil-visual-block-overlays
      (mapc 'delete-overlay evil-visual-block-overlays)
      (setq evil-visual-block-overlays nil)))
   ((eq evil-visual-type 'block)
    (when evil-visual-overlay
      (evil-visual-highlight -1))
    (evil-visual-highlight-block
     evil-visual-beginning
     evil-visual-end))
   (t
    (when evil-visual-block-overlays
      (evil-visual-highlight -1))
    (if evil-visual-overlay
        (move-overlay evil-visual-overlay
                      evil-visual-beginning evil-visual-end)
      (setq evil-visual-overlay
            (make-overlay evil-visual-beginning evil-visual-end)))
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
            (setq new (cons overlay new)))))
        (forward-line 1))
      ;; display overlays
      (dolist (overlay new)
        (overlay-put overlay 'face 'region)
        (overlay-put overlay 'priority 99))
      ;; trim old overlays
      (dolist (overlay old)
        (delete-overlay overlay))
      (set overlays (nreverse new)))))

(defun evil-visual-range ()
  "Return the Visual selection as a range.
This is a list (BEG END TYPE PROPERTIES...), where BEG is the
beginning of the selection, END is the end of the selection,
TYPE is the selection's type, and PROPERTIES is a property list
of miscellaneous selection attributes."
  (apply #'evil-range
         evil-visual-beginning evil-visual-end
         evil-visual-type evil-visual-properties))

(defun evil-visual-direction ()
  "Return direction of Visual selection.
The direction is -1 if point precedes mark and 1 otherwise.
See also the variable `evil-visual-direction', which holds
the direction of the last selection."
  (let* ((point (point))
         (mark (or (mark t) point)))
    (if (< point mark) -1 1)))

;; recognizes user changes, e.g., customizing
;; `evil-visual-char' to `exclusive'
(defun evil-visual-alist ()
  "Return an association list from types to selection functions."
  (mapcar (lambda (e)
            (cons (symbol-value (cdr-safe e)) (cdr-safe e)))
          evil-visual-alist))

(defun evil-visual-selection-type (selection)
  "Return the type of SELECTION."
  (or (symbol-value (cdr-safe (assq selection evil-visual-alist)))
      selection))

(defun evil-visual-selection-function (type)
  "Return a selection function for TYPE.
Default to `evil-visual-make-region'."
  (or (cdr (assq type evil-visual-alist))
      (cdr (assq type (evil-visual-alist)))
      ;; generic selection function
      'evil-visual-make-region))

(evil-define-command evil-visual-restore ()
  "Restore previous selection."
  :keep-visual t
  (interactive)
  (let* ((point (point))
         (mark (or (mark t) point))
         (dir evil-visual-direction)
         (type evil-visual-type)
         range)
    (unless (evil-visual-state-p)
      (cond
       ;; No previous selection.
       ((or (null type)
            (null evil-visual-mark)
            (null evil-visual-point)))
       ;; If the type was one-to-one, it is preferable to infer
       ;; point and mark from the selection's boundaries. The reason
       ;; is that a destructive operation may displace the markers
       ;; inside the selection.
       ((evil-type-property type :one-to-one)
        (setq range (evil-contract-range (evil-visual-range))
              mark (evil-range-beginning range)
              point (evil-range-end range))
        (when (< dir 0)
          (evil-swap mark point)))
       ;; If the type wasn't one-to-one, we have to restore the
       ;; selection on the basis of the previous point and mark.
       (t
        (setq mark evil-visual-mark
              point evil-visual-point)))
      (evil-visual-make-selection mark point type))))

(evil-define-command evil-visual-exchange-corners ()
  "Rearrange corners in Visual Block mode.

        M---+           +---M
        |   |    <=>    |   |
        +---P           P---+

For example, if mark is in the upper left corner and point
in the lower right, this function puts mark in the upper right
corner and point in the lower left."
  :keep-visual t
  (interactive)
  (cond
   ((eq evil-visual-type evil-visual-block)
    (let* ((point (point))
           (mark (or (mark t) point))
           (point-col (current-column))
           (mark-col (save-excursion
                       (goto-char mark)
                       (current-column)))
           (mark (save-excursion
                   (goto-char mark)
                   (evil-move-to-column point-col)
                   (point)))
           (point (save-excursion
                    (goto-char point)
                    (evil-move-to-column mark-col)
                    (point))))
      (evil-visual-refresh evil-visual-block mark point)))
   (t
    (evil-exchange-point-and-mark)
    (evil-visual-refresh))))

(defun evil-visual-block-corner (&optional corner point mark)
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
         (corner (symbol-name
                  (or corner
                      (and (overlayp evil-visual-overlay)
                           (overlay-get evil-visual-overlay
                                        :corner))
                      'upper-left)))
         (point-col (save-excursion
                      (goto-char point)
                      (current-column)))
         (mark-col (save-excursion
                     (goto-char mark)
                     (current-column)))
         horizontal vertical)
    (cond
     ((= point-col mark-col)
      (setq horizontal
            (or (and (string-match "left\\|right" corner)
                     (match-string 0 corner))
                "left")))
     ((< point-col mark-col)
      (setq horizontal "left"))
     ((> point-col mark-col)
      (setq horizontal "right")))
    (cond
     ((= (line-number-at-pos point)
         (line-number-at-pos mark))
      (setq vertical
            (or (and (string-match "upper\\|lower" corner)
                     (match-string 0 corner))
                "upper")))
     ((< point mark)
      (setq vertical "upper"))
     ((> point mark)
      (setq vertical "lower")))
    (intern (format "%s-%s" vertical horizontal))))

(evil-define-command evil-visual-rotate (corner &optional beg end type)
  "In Visual Block selection, put point in CORNER.
Corner may be one of `upper-left', `upper-right', `lower-left'
and `lower-right':

        upper-left +---+ upper-right
                   |   |
        lower-left +---+ lower-right

When called interactively, the selection is rotated blockwise."
  :keep-visual t
  (interactive
   (let ((corners '(upper-left upper-right lower-right lower-left)))
     (list (or (cadr (memq (evil-visual-block-corner) corners))
               'upper-left))))
  (let* ((beg (or beg (point)))
         (end (or end (mark t) beg))
         (type (or type evil-visual-type))
         range)
    (cond
     ((eq type 'block)
      (setq range (evil-block-rotate beg end :corner corner)
            beg (pop range)
            end (pop range))
      (unless (eq corner (evil-visual-block-corner corner beg end))
        (evil-swap beg end))
      (goto-char beg)
      (evil-move-mark end)
      (when (evil-visual-state-p)
        (evil-visual-refresh evil-visual-block
                             nil nil :corner corner)))
     ((memq corner '(upper-right lower-right))
      (goto-char (max beg end))
      (evil-move-mark (min beg end)))
     (t
      (goto-char (min beg end))
      (evil-move-mark (max beg end))))))

(provide 'evil-visual)

;;; evil-visual.el ends here
