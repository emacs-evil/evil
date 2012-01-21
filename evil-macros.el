;;;; Macros

(require 'evil-common)
(require 'evil-states)
(require 'evil-repeat)

(defun evil-motion-range (motion &optional count type)
  "Execute a motion and return the buffer positions.
The return value is a list (BEG END TYPE)."
  (let ((opoint   (point))
        (omark    (mark t))
        (omactive (and (boundp 'mark-active) mark-active))
        (obuffer  (current-buffer))
        (evil-motion-marker (move-marker (make-marker) (point)))
        range)
    (evil-narrow-to-field
      (evil-save-transient-mark
        (evil-transient-mark 1)
        (unwind-protect
            (let ((current-prefix-arg count)
                  ;; Store type in global variable `evil-this-type'.
                  ;; If necessary, motions can change their type
                  ;; during execution by setting this variable.
                  (evil-this-type
                   (or type (evil-type motion 'exclusive))))
              (condition-case err
                  (let ((repeat-type (evil-repeat-type motion t)))
                    (if (functionp repeat-type)
                        (funcall repeat-type 'pre))
                    (unless (with-local-quit
                              (setq range (call-interactively motion))
                              t)
                      (evil-repeat-abort)
                      (setq quit-flag t))
                    (if (functionp repeat-type)
                        (funcall repeat-type 'post)))
                (error (prog1 nil
                         (evil-repeat-abort)
                         (setq evil-this-type 'exclusive
                               evil-write-echo-area t)
                         (message (error-message-string err)))))
              (cond
               ;; the motion returned a range
               ((evil-range-p range))
               ;; the motion made a Visual selection
               ((evil-visual-state-p)
                (setq range (evil-visual-range)))
               ;; the motion made an active region
               ((region-active-p)
                (setq range (evil-range (region-beginning)
                                        (region-end)
                                        evil-this-type)))
               ;; default: range from previous position to current
               (t
                (setq range (evil-expand-range
                             (evil-normalize evil-motion-marker
                                             (point)
                                             evil-this-type)))))
              (unless (or (null type) (eq (evil-type range) type))
                (evil-set-type range type)
                (evil-expand-range range))
              (evil-set-range-properties range nil)
              range)
          ;; restore point and mark like `save-excursion',
          ;; but only if the motion hasn't disabled the operator
          (unless evil-inhibit-operator
            (set-buffer obuffer)
            (evil-move-mark omark)
            (goto-char opoint))
          ;; delete marker so it doesn't slow down editing
          (move-marker evil-motion-marker nil))))))

(defmacro evil-define-motion (motion args &rest body)
  "Define an motion command MOTION.

\(fn MOTION (COUNT ARGS...) DOC [[KEY VALUE]...] BODY...)"
  (declare (indent defun)
           (debug (&define name lambda-list
                           [&optional stringp]
                           [&rest keywordp sexp]
                           [&optional ("interactive" [&rest form])]
                           def-body)))
  (let (arg doc interactive key keys type)
    (when args
      (setq args `(&optional ,@(delq '&optional args))
            ;; the count is either numerical or nil
            interactive '("<c>")))
    ;; collect docstring
    (when (and (> (length body) 1)
               (or (eq (car-safe (car-safe body)) 'format)
                   (stringp (car-safe body))))
      (setq doc (pop body)))
    ;; collect keywords
    (setq keys (plist-put keys :repeat 'motion))
    (while (keywordp (car-safe body))
      (setq key (pop body)
            arg (pop body)
            keys (plist-put keys key arg)))
    ;; collect `interactive' specification
    (when (eq (car-safe (car-safe body)) 'interactive)
      (setq interactive (cdr (pop body))))
    (when interactive
      (setq interactive (apply #'evil-interactive-form interactive))
      (setq keys (evil-concat-plists keys (cdr-safe interactive))
            interactive (car-safe interactive)))
    ;; macro expansion
    `(progn
       ;; refresh echo area in Eldoc mode
       (when ',motion
         (eval-after-load 'eldoc
           '(eldoc-add-command ',motion)))
       (evil-define-command ,motion (,@args)
         ,@(when doc `(,doc))          ; avoid nil before `interactive'
         ,@keys
         :keep-visual t
         (interactive
          (progn
            (when (evil-get-command-property ',motion :jump)
              (unless (or (evil-visual-state-p)
                          (evil-operator-state-p))
                (evil-set-jump)))
            ,interactive))
         ,@body))))

(defmacro evil-define-union-move (name args &rest moves)
  "Create a movement function named NAME.
The function moves to the nearest object boundary defined by one
of the movement function in MOVES, which is a list where each
element has the form \(FUNC PARAMS... COUNT).

COUNT is a variable which is bound to 1 or -1, depending on the
direction. In each iteration, the function calls each move in
isolation and settles for the nearest position. If unable to move
further, the return value is the number of iterations that could
not be performed.

\(fn NAME (COUNT) MOVES...)"
  (declare (indent defun)
           (debug (&define name lambda-list
                           [&optional stringp]
                           def-body)))
  (let* ((var (or (car-safe args) 'var))
         (doc (when (stringp (car-safe moves))
                (pop moves)))
         (moves (mapcar #'(lambda (move)
                            `(save-excursion
                               ;; don't include failing moves
                               (when (zerop ,move)
                                 (point))))
                        moves)))
    `(evil-define-motion ,name (count)
       ,@(when doc `(,doc))
       (evil-motion-loop (,var (or count 1))
         (if (> ,var 0)
             (evil-goto-min ,@moves)
           (evil-goto-max ,@moves))))))

(defmacro evil-narrow-to-line (&rest body)
  "Narrow BODY to the current line."
  (declare (indent defun)
           (debug t))
  `(save-restriction
     (narrow-to-region
      (line-beginning-position)
      (if (and evil-move-cursor-back
               (not (evil-visual-state-p))
               (not (evil-operator-state-p)))
          (max (line-beginning-position)
               (1- (line-end-position)))
        (line-end-position)))
     (evil-signal-without-movement
       (condition-case nil
           (progn ,@body)
         (beginning-of-buffer
          (error "Beginning of line"))
         (end-of-buffer
          (error "End of line"))))))

;; we don't want line boundaries to trigger the debugger
;; when `debug-on-error' is t
(add-to-list 'debug-ignored-errors "^Beginning of line$")
(add-to-list 'debug-ignored-errors "^End of line$")

(defmacro evil-narrow-to-line-if (cond &rest body)
  "Narrow BODY to the current line if COND yields non-nil."
  (declare (indent 1)
           (debug t))
  `(if ,cond
       (evil-narrow-to-line ,@body)
     ,@body))

(defun evil-eobp ()
  "Whether point is at end-of-buffer with regard to end-of-line."
  (or (eobp)
      (and (evil-normal-state-p)
           (= (point) (1- (point-max)))
           (not (eolp)))))

(defun evil-move-beginning (count forward &optional backward)
  "Move to the beginning of the COUNT next object.
If COUNT is negative, move to the COUNT previous object.
FORWARD is a function which moves to the end of the object, and
BACKWARD is a function which moves to the beginning.
If one is unspecified, the other is used with a negative argument."
  (let* ((count (or count 1))
         (backward (or backward
                       #'(lambda (count)
                           (funcall forward (- count)))))
         (forward (or forward
                      #'(lambda (count)
                          (funcall backward (- count)))))
         (opoint (point)))
    (cond
     ((< count 0)
      (when (bobp)
        (signal 'beginning-of-buffer nil))
      (unwind-protect
          (evil-motion-loop (nil count count)
            (funcall backward 1))
        (unless (zerop count)
          (goto-char (point-min)))))
     ((> count 0)
      (when (evil-eobp)
        (signal 'end-of-buffer nil))
      ;; Do we need to move past the current object?
      (when (<= (save-excursion
                  (funcall forward 1)
                  (funcall backward 1)
                  (point))
                opoint)
        (setq count (1+ count)))
      (unwind-protect
          (evil-motion-loop (nil count count)
            (funcall forward 1))
        (if (zerop count)
            ;; go back to beginning of object
            (funcall backward 1)
          (goto-char (point-max)))))
     (t
      count))))

(defun evil-move-end (count forward &optional backward inclusive)
  "Move to the end of the COUNT next object.
If COUNT is negative, move to the COUNT previous object.
FORWARD is a function which moves to the end of the object, and
BACKWARD is a function which moves to the beginning.
If one is unspecified, the other is used with a negative argument.
If INCLUSIVE is non-nil, then point is placed at the last character
of the object; otherwise it is placed at the end of the object."
  (let* ((count (or count 1))
         (backward (or backward
                       #'(lambda (count)
                           (funcall forward (- count)))))
         (forward (or forward
                      #'(lambda (count)
                          (funcall backward (- count)))))
         (opoint (point)))
    (cond
     ((< count 0)
      (when (bobp)
        (signal 'beginning-of-buffer nil))
      ;; Do we need to move past the current object?
      (when (>= (save-excursion
                  (funcall backward 1)
                  (funcall forward 1)
                  (point))
                (if inclusive
                    (1+ opoint)
                  opoint))
        (setq count (1- count)))
      (unwind-protect
          (evil-motion-loop (nil count count)
            (funcall backward 1))
        (if (not (zerop count))
            (goto-char (point-min))
          ;; go to end of object
          (funcall forward 1)
          (when inclusive
            (unless (bobp) (backward-char)))
          (when (or (evil-normal-state-p)
                    (evil-motion-state-p))
            (evil-adjust-cursor t)))))
     ((> count 0)
      (when (evil-eobp)
        (signal 'end-of-buffer nil))
      (when inclusive
        (forward-char))
      (unwind-protect
          (evil-motion-loop (nil count count)
            (funcall forward 1))
        (if (not (zerop count))
            (goto-char (point-max))
          (when inclusive
            (unless (bobp) (backward-char)))
          (when (or (evil-normal-state-p)
                    (evil-motion-state-p))
            (evil-adjust-cursor t)))))
     (t
      count))))

(defmacro evil-define-text-object (object args &rest body)
  "Define a text object command OBJECT.
BODY should return a range (BEG END) to the right of point
if COUNT is positive, and to the left of it if negative.

\(fn OBJECT (COUNT) DOC [[KEY VALUE]...] BODY...)"
  (declare (indent defun)
           (debug (&define name lambda-list
                           [&optional stringp]
                           [&rest keywordp sexp]
                           def-body)))
  (let* ((args (delq '&optional args))
         (count (or (pop args) 'count))
         (args (when args `(&optional ,@args)))
         arg doc key keys)
    ;; collect docstring
    (when (stringp (car-safe body))
      (setq doc (pop body)))
    ;; collect keywords
    (setq keys (plist-put keys :extend-selection t))
    (while (keywordp (car-safe body))
      (setq key (pop body)
            arg (pop body)
            keys (plist-put keys key arg)))
    ;; macro expansion
    `(evil-define-motion ,object (,count ,@args)
       ,@(when doc `(,doc))
       ,@keys
       (setq ,count (or ,count 1))
       (when (/= ,count 0)
         (let ((type (evil-type ',object evil-visual-char))
               (extend (evil-get-command-property
                        ',object :extend-selection
                        ',(plist-get keys :extend-selection)))
               (dir evil-visual-direction)
               mark point range region selection temp)
           (cond
            ;; Visual state: extend the current selection
            ((and (evil-visual-state-p)
                  (evil-called-interactively-p))
             ;; if we are at the beginning of the Visual selection,
             ;; go to the left (negative COUNT); if at the end,
             ;; go to the right (positive COUNT)
             (setq dir evil-visual-direction
                   ,count (* ,count dir)
                   region (evil-range (mark t) (point))
                   selection (evil-visual-range))
             (when extend
               (setq range (evil-range (point) (point) type)))
             ;; select the object under point
             (let ((,count dir))
               (setq temp (progn ,@body))
               (unless (evil-range-p temp)
                 ;; At the end of the buffer?
                 ;; Try the other direction.
                 (setq ,count (- dir)
                       temp (progn ,@body))))
             (when (and (evil-range-p temp)
                        (not (evil-subrange-p temp selection))
                        (or (not extend)
                            (if (< dir 0)
                                (>= (evil-range-end temp)
                                    (evil-range-end selection))
                              (<= (evil-range-beginning temp)
                                  (evil-range-beginning selection)))))
               ;; found an unselected object under point:
               ;; decrease COUNT by one and save the result
               (setq ,count (if (< ,count 0) (1+ ,count) (1- ,count)))
               (if extend
                   (setq range (evil-range-union temp range))
                 (setq range temp)
                 (evil-set-type range (evil-type range type))))
             ;; now look for remaining objects
             (when (/= ,count 0)
               ;; expand the Visual selection so point is outside it
               (evil-visual-make-selection (mark t) (point) type)
               (evil-visual-expand-region)
               (setq selection (evil-visual-range))
               (if (< dir 0)
                   (evil-goto-min (evil-range-beginning range)
                                  (evil-range-beginning selection))
                 (evil-goto-max (evil-range-end range)
                                (evil-range-end selection)))
               (setq temp (progn ,@body))
               (when (evil-range-p temp)
                 ;; if the previous attempts failed, then enlarge
                 ;; the selection by one character as a last resort
                 (when (evil-subrange-p temp selection)
                   (if (< dir 0)
                       (backward-char)
                     (forward-char))
                   (setq temp (progn ,@body)))
                 (if extend
                     (setq range (evil-range-union temp range))
                   (setq range temp))
                 (evil-set-type range (evil-type range type)))
               (evil-visual-contract-region))
             (when (evil-range-p range)
               ;; Find the union of the range and the selection.
               ;; Actually, this uses point and mark rather than the
               ;; selection boundaries to prevent the object from
               ;; unnecessarily overwriting the mark's position;
               ;; if the selection is larger than the object,
               ;; only point needs to move.
               (setq range (evil-contract-range range))
               (when extend
                 (setq range (evil-range-union range region)))
               ;; the beginning is mark and the end is point
               ;; unless the selection goes the other way
               (setq mark  (evil-range-beginning range)
                     point (evil-range-end range)
                     type  (evil-type range type))
               (when (< dir 0)
                 (evil-swap mark point))
               ;; select the union
               (evil-visual-make-selection mark point type)))
            ;; not Visual state: return a pair of buffer positions
            (t
             (setq range (progn ,@body))
             (unless (evil-range-p range)
               (setq ,count (- ,count)
                     range (progn ,@body)))
             (when (evil-range-p range)
               (setq selection (evil-range (point) (point) type))
               (if extend
                   (setq range (evil-range-union range selection))
                 (evil-set-type range (evil-type range type)))
               ;; ensure the range is properly expanded
               (evil-contract-range range)
               (evil-expand-range range)
               (evil-set-range-properties range nil)
               range))))))))

(defmacro evil-define-operator (operator args &rest body)
  "Define an operator command OPERATOR.

\(fn OPERATOR (BEG END ARGS...) DOC [[KEY VALUE]...] BODY...)"
  (declare (indent defun)
           (debug (&define name lambda-list
                           [&optional stringp]
                           [&rest keywordp sexp]
                           [&optional ("interactive" [&rest form])]
                           def-body)))
  (let* ((args (delq '&optional args))
         (interactive (if (> (length args) 2) '("<R>") '("<r>")))
         (args (if (> (length args) 2)
                   `(,(nth 0 args) ,(nth 1 args)
                     &optional ,@(nthcdr 2 args))
                 args))
         arg doc key keys visual)
    ;; collect docstring
    (when (and (> (length body) 1)
               (or (eq (car-safe (car-safe body)) 'format)
                   (stringp (car-safe body))))
      (setq doc (pop body)))
    ;; collect keywords
    (setq keys (plist-put keys :move-point t))
    (while (keywordp (car-safe body))
      (setq key (pop body)
            arg (pop body))
      (cond
       ((eq key :keep-visual)
        (setq visual arg))
       (t
        (setq keys (plist-put keys key arg)))))
    ;; collect `interactive' specification
    (when (eq (car-safe (car-safe body)) 'interactive)
      (setq interactive (cdr-safe (pop body))))
    ;; transform extended interactive specs
    (setq interactive (apply #'evil-interactive-form interactive))
    (setq keys (evil-concat-plists keys (cdr-safe interactive))
          interactive (car-safe interactive))
    ;; macro expansion
    `(evil-define-command ,operator ,args
       ,@(when doc `(,doc))
       ,@keys
       :keep-visual t
       :suppress-operator t
       (interactive
        (let* ((evil-operator-range-motion
                (when (evil-has-command-property-p ',operator :motion)
                  ;; :motion nil is equivalent to :motion undefined
                  (or (evil-get-command-property ',operator :motion)
                      #'undefined)))
               (evil-operator-range-type
                (evil-get-command-property ',operator :type))
               (orig (point))
               (state evil-state)
               evil-operator-range-beginning
               evil-operator-range-end
               evil-inhibit-operator)
          (setq evil-inhibit-operator-value nil
                evil-this-operator this-command)
          (unwind-protect
              ,interactive
            (setq orig (point)
                  evil-inhibit-operator-value evil-inhibit-operator)
            (if ,visual
                (when (evil-visual-state-p)
                  (evil-visual-expand-region))
              (when (evil-visual-state-p)
                (evil-exit-visual-state))
              (when (region-active-p)
                (evil-active-region -1)))
            (if (or (evil-visual-state-p state)
                    (and (evil-get-command-property ',operator :move-point)
                         evil-operator-range-beginning
                         evil-operator-range-end))
                (evil-visual-rotate 'upper-left
                                    evil-operator-range-beginning
                                    evil-operator-range-end
                                    evil-operator-range-type)
              (goto-char orig)))))
       (unwind-protect
           (let ((evil-inhibit-operator evil-inhibit-operator-value))
             (unless (and evil-inhibit-operator
                          (evil-called-interactively-p))
               ,@body))
         (setq evil-inhibit-operator-value nil)))))

;; this is used in the `interactive' specification of an operator command
(defun evil-operator-range (&optional return-type)
  "Read a motion from the keyboard and return its buffer positions.
The return value is a list (BEG END) or (BEG END TYPE),
depending on RETURN-TYPE. Instead of reading from the keyboard,
a predefined motion may be specified with MOTION. Likewise,
a predefined type may be specified with TYPE."
  (let ((motion (or evil-operator-range-motion
                    (when (and (fboundp 'evil-ex-p) (evil-ex-p))
                      #'evil-line)))
        (type evil-operator-range-type)
        (range (evil-range (point) (point)))
        command count modifier)
    (evil-save-echo-area
      (cond
       ;; Visual selection
       ((evil-visual-state-p)
        (setq range (evil-visual-range)))
       ;; Ex mode
       ((and (fboundp 'evil-ex-p)
             (evil-ex-p)
             evil-ex-range)
        (setq range evil-ex-range))
       ;; active region
       ((region-active-p)
        (setq range (evil-range (region-beginning)
                                (region-end)
                                (or evil-this-type 'exclusive))))
       (t
        ;; motion
        (evil-save-state
          (unless motion
            (evil-change-state 'operator)
            ;; Make linewise operator shortcuts. E.g., "d" yields the
            ;; shortcut "dd", and "g?" yields shortcuts "g??" and "g?g?".
            (let ((keys (nth 2 (evil-extract-count (this-command-keys)))))
              (setq keys (listify-key-sequence keys))
              (dotimes (var (length keys))
                (define-key evil-operator-shortcut-map
                  (vconcat (nthcdr var keys)) 'evil-line)))
            ;; read motion from keyboard
            (setq command (evil-read-motion motion)
                  motion (nth 0 command)
                  count (nth 1 command)
                  type (or type (nth 2 command))))
          (cond
           ;; ESC cancels the current operator
           ;; TODO: is there a better way to detect this canceling?
           ((memq motion '(nil evil-esc))
            (when (fboundp 'evil-repeat-abort)
              (evil-repeat-abort))
            (setq quit-flag t))
           ((evil-get-command-property motion :suppress-operator)
            (when (fboundp 'evil-repeat-abort)
              (evil-repeat-abort))
            (setq quit-flag t))
           ((eq motion #'undefined)
            (setq range (if return-type '(nil nil nil) '(nil nil))
                  motion nil))
           (evil-repeat-count
            (setq count evil-repeat-count
                  ;; only the first operator's count is overwritten
                  evil-repeat-count nil))
           ((or count current-prefix-arg)
            ;; multiply operator count and motion count together
            (setq count
                  (* (prefix-numeric-value count)
                     (prefix-numeric-value current-prefix-arg)))))
          (when motion
            (let ((evil-state 'operator))
              ;; calculate motion range
              (setq range (evil-motion-range
                           motion
                           count
                           type))
              (evil-set-marker ?. (evil-range-end range) t)))
          ;; update global variables
          (setq evil-this-motion motion
                evil-this-motion-count count
                type (evil-type range type)
                evil-this-type type))))
      (when (evil-range-p range)
        (unless (or (null type) (eq (evil-type range) type))
          (evil-set-type range type)
          (evil-expand-range range))
        (evil-set-range-properties range nil)
        (unless return-type
          (evil-set-type range nil))
        (setq evil-operator-range-beginning (evil-range-beginning range)
              evil-operator-range-end (evil-range-end range)
              evil-operator-range-type (evil-type range)))
      range)))

(defmacro evil-define-type (type doc &rest body)
  "Define type TYPE.
DOC is a general description and shows up in all docstrings.
It is followed by a list of keywords and functions:

:expand FUNC     Expansion function. This function should accept
                 two positions in the current buffer, BEG and END,
                 and return a pair of expanded buffer positions.
:contract FUNC   The opposite of :expand, optional.
:one-to-one BOOL Whether expansion is one-to-one. This means that
                 :expand followed by :contract always returns the
                 original range.
:normalize FUNC  Normalization function, optional. This function should
                 accept two unexpanded positions and adjust them before
                 expansion. May be used to deal with buffer boundaries.
:string FUNC     Description function. This takes two buffer positions
                 and returns a human-readable string, for example,
                 \"2 lines\".

Further keywords and functions may be specified. These are assumed to
be transformations on buffer positions, like :expand and :contract.

\(fn TYPE DOC [[KEY FUNC]...])"
  (declare (indent defun)
           (debug (&define name
                           [&optional stringp]
                           [&rest [keywordp function-form]])))
  (let (args defun-forms func key name plist string sym val)
    ;; standard values
    (setq plist (plist-put plist :one-to-one t))
    ;; keywords
    (while (keywordp (car-safe body))
      (setq key (pop body)
            val (pop body))
      (if (plist-member plist key) ; not a function
          (setq plist (plist-put plist key val))
        (setq func val
              sym (intern (replace-regexp-in-string
                           "^:" "" (symbol-name key)))
              name (intern (format "evil-%s-%s" type sym))
              args (car (cdr-safe func))
              string (car (cdr (cdr-safe func)))
              string (if (stringp string)
                         (format "%s\n\n" string) "")
              plist (plist-put plist key `',name))
        (add-to-list
         'defun-forms
         (cond
          ((eq key :string)
           `(defun ,name (beg end &rest properties)
              ,(format "Return size of %s from BEG to END \
with PROPERTIES.\n\n%s%s" type string doc)
              (let ((beg (evil-normalize-position beg))
                    (end (evil-normalize-position end))
                    (type ',type)
                    plist range)
                (when (and beg end)
                  (save-excursion
                    (evil-sort beg end)
                    (unless (plist-get properties :expanded)
                      (setq range (apply #'evil-expand
                                         beg end type properties)
                            beg (evil-range-beginning range)
                            end (evil-range-end range)
                            type (evil-type range type)
                            plist (evil-range-properties range))
                      (setq properties
                            (evil-concat-plists properties plist)))
                    (or (apply #',func beg end
                               (when ,(> (length args) 2)
                                 properties))
                        ""))))))
          (t
           `(defun ,name (beg end &rest properties)
              ,(format "Perform %s transformation on %s from BEG to END \
with PROPERTIES.\n\n%s%s" sym type string doc)
              (let ((beg (evil-normalize-position beg))
                    (end (evil-normalize-position end))
                    (type ',type)
                    plist range)
                (when (and beg end)
                  (save-excursion
                    (evil-sort beg end)
                    (when (memq ,key '(:expand :contract))
                      (setq properties
                            (plist-put properties
                                       :expanded
                                       ,(eq key :expand))))
                    (setq range (or (apply #',func beg end
                                           (when ,(> (length args) 2)
                                             properties))
                                    (apply #'evil-range
                                           beg end type properties))
                          beg (evil-range-beginning range)
                          end (evil-range-end range)
                          type (evil-type range type)
                          plist (evil-range-properties range))
                    (setq properties
                          (evil-concat-plists properties plist))
                    (apply #'evil-range beg end type properties)))))))
         t)))
    ;; :one-to-one requires both or neither of :expand and :contract
    (when (plist-get plist :expand)
      (setq plist (plist-put plist :one-to-one
                             (and (plist-get plist :contract)
                                  (plist-get plist :one-to-one)))))
    `(progn
       (evil-put-property 'evil-type-properties ',type ,@plist)
       ,@defun-forms
       ',type)))

(defmacro evil-define-interactive-code (code &rest body)
  "Define an interactive code.
PROMPT, if given, is the remainder of the interactive string
up to the next newline. Command properties may be specified
via KEY-VALUE pairs. BODY should evaluate to a list of values.

\(fn CODE (PROMPT) [[KEY VALUE]...] BODY...)"
  (declare (indent defun))
  (let* ((args (when (and (> (length body) 1)
                          (listp (car-safe body)))
                 (pop body)))
         (doc (when (stringp (car-safe body)) (pop body)))
         func properties)
    (while (keywordp (car-safe body))
      (setq properties
            (append properties (list (pop body) (pop body)))))
    (cond
     (args
      (setq func `(lambda ,args
                    ,@(when doc `(,doc))
                    ,@body)))
     ((> (length body) 1)
      (setq func `(progn ,@body)))
     (t
      (setq func (car body))))
    `(eval-and-compile
       (let* ((code ,code)
              (entry (assoc code evil-interactive-alist))
              (value (cons ',func ',properties)))
         (if entry
             (setcdr entry value)
           (push (cons code value) evil-interactive-alist))
         code))))

;;; Highlighting

(when (fboundp 'font-lock-add-keywords)
  (font-lock-add-keywords
   'emacs-lisp-mode
   ;; Match all `evil-define-' forms except `evil-define-key'.
   ;; (In the interests of speed, this expression is incomplete
   ;; and does not match all three-letter words.)
   '(("(\\(evil-\\(?:ex-\\)?define-\\(?:[^ k][^ e][^ y]\\|[-[:word:]]\\{4,\\}\\)\\)\
\\>[ \f\t\n\r\v]*\\(\\sw+\\)?"
      (1 font-lock-keyword-face)
      (2 font-lock-function-name-face nil t))
     ("(\\(evil-\\(?:narrow\\|save\\|with\\(?:out\\)?\\)-[-[:word:]]+\\)\\>"
      1 font-lock-keyword-face)
     ("(\\(evil-\\(?:[-[:word:]]\\)*loop\\)\\>"
      1 font-lock-keyword-face))))

(provide 'evil-macros)

;;; evil-macros.el ends here
