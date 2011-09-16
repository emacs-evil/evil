;;;; Type system

;; A type defines a transformation on a pair of buffer positions.
;; Types are used by Visual state (character/line/block selection)
;; and Operator-Pending state (character/line/block motions).
;;
;; The basic transformation is "expansion". For example, the `line'
;; type "expands" a pair of positions to whole lines by moving the
;; first position to the beginning of its line and the last position
;; to the end of its line. That expanded selection is what the rest
;; of Emacs sees and acts on.
;;
;; An optional transformation is "contraction", which is the opposite
;; of expansion (assuming the expansion is one-to-one). The
;; `inclusive' type, which increases the last position by one, is
;; one-to-one and contractable. The `line' type is not one-to-one
;; as it may expand multiple positions to the same lines, so it
;; has no contraction procedure.
;;
;; Another optional transformation is "normalization", which takes
;; two unexpanded positions and adjusts them before expansion.
;; This is useful for cleaning up "invalid" positions.
;;
;; Types are defined at the end of this file using the macro
;; `evil-define-type'.

(require 'evil-common)

(defun evil-type (object &optional default)
  "Return the type of OBJECT, or DEFAULT if none."
  (let (type)
    (cond
     ((overlayp object)
      (setq type (overlay-get object :type)))
     ((evil-range-p object)
      (setq type (nth 2 object)))
     ((listp object)
      (setq type (plist-get object :type)))
     ;; command
     ((commandp object)
      (setq type (evil-get-command-property object :type)))
     ((symbolp object)
      (setq type (get object 'type))))
    (setq type (or type default))
    (and (evil-type-p type) type)))

(defun evil-set-type (object type)
  "Set the type of OBJECT to TYPE.
For example, (evil-set-type 'next-line 'line)
will make `line' the type of the `next-line' command."
  (cond
   ((overlayp object)
    (overlay-put object :type type))
   ((evil-range-p object)
    (evil-set-range-type object type))
   ((listp object)
    (plist-put object :type type))
   ((commandp object)
    (evil-add-command-properties object :type type))
   ((symbolp object)
    (put object 'type type)))
  object)

(defun evil-type-property (type prop)
  "Return property PROP for TYPE."
  (evil-get-property evil-type-properties type prop))

(defun evil-type-p (sym)
  "Whether SYM is the name of a type."
  (assq sym evil-type-properties))

(defun evil-range (beg end &optional type &rest properties)
  "Return a list (BEG END [TYPE] PROPERTIES...).
BEG and END are buffer positions (numbers or markers),
TYPE is a type as per `evil-type-p', and PROPERTIES is
a property list."
  (let ((beg (if (markerp beg) (marker-position beg) beg))
        (end (if (markerp end) (marker-position end) end))
        (point-min (point-min))
        (point-max (point-max)))
    ;; BEG and END may not exceed the buffer boundaries
    (evil-sort point-min beg end point-max)
    (append (list beg end)
            (when (evil-type-p type)
              (list type))
            properties)))

(defun evil-range-p (object)
  "Whether OBJECT is a range."
  (and (listp object)
       (>= (length object) 2)
       (numberp (nth 0 object))
       (numberp (nth 1 object))))

(defun evil-range-beginning (range)
  "Return beginning of RANGE."
  (when (evil-range-p range)
    (let ((beg (nth 0 range))
          (end (nth 1 range))
          (point-min (point-min))
          (point-max (point-max)))
      ;; `beg' may not exceed the buffer boundaries
      (evil-sort point-min beg end point-max)
      beg)))

(defun evil-range-end (range)
  "Return end of RANGE."
  (when (evil-range-p range)
    (let ((beg (nth 0 range))
          (end (nth 1 range))
          (point-min (point-min))
          (point-max (point-max)))
      ;; `end' may not exceed the buffer boundaries
      (evil-sort point-min beg end point-max)
      end)))

(defun evil-range-properties (range)
  "Return properties of RANGE."
  (when (evil-range-p range)
    (if (evil-type range)
        (nthcdr 3 range)
      (nthcdr 2 range))))

(defun evil-copy-range (range)
  "Return a copy of RANGE."
  (copy-sequence range))

(defun evil-set-range (range &optional beg end type &rest properties)
  "Set RANGE to have beginning BEG and end END.
The TYPE and additional PROPERTIES may also be specified.
If an argument is nil, it's not used; the previous value is retained.
See also `evil-set-range-beginning', `evil-set-range-end',
`evil-set-range-type' and `evil-set-range-properties'."
  (when (evil-range-p range)
    (let ((beg (or beg (evil-range-beginning range)))
          (end (or end (evil-range-end range)))
          (type (or type (evil-type range)))
          (plist (evil-range-properties range))
          (point-min (point-min))
          (point-max (point-max)))
      (evil-sort point-min beg end point-max)
      (while properties
        (setq plist (plist-put plist (pop properties) (pop properties))))
      (evil-set-range-beginning range beg)
      (evil-set-range-end range end)
      (evil-set-range-type range type)
      (evil-set-range-properties range plist)
      range)))

(defun evil-set-range-beginning (range beg &optional copy)
  "Set RANGE's beginning to BEG.
If COPY is non-nil, return a copy of RANGE."
  (when copy
    (setq range (evil-copy-range range)))
  (setcar range beg)
  range)

(defun evil-set-range-end (range end &optional copy)
  "Set RANGE's end to END.
If COPY is non-nil, return a copy of RANGE."
  (when copy
    (setq range (evil-copy-range range)))
  (setcar (cdr range) end)
  range)

(defun evil-set-range-type (range type &optional copy)
  "Set RANGE's type to TYPE.
If COPY is non-nil, return a copy of RANGE."
  (when copy
    (setq range (evil-copy-range range)))
  (if type
      (setcdr (cdr range)
              (cons type (evil-range-properties range)))
    (setcdr (cdr range) (evil-range-properties range)))
  range)

(defun evil-set-range-properties (range properties &optional copy)
  "Set RANGE's properties to PROPERTIES.
If COPY is non-nil, return a copy of RANGE."
  (when copy
    (setq range (evil-copy-range range)))
  (if (evil-type range)
      (setcdr (cdr (cdr range)) properties)
    (setcdr (cdr range) properties))
  range)

(defun evil-range-union (range1 range2 &optional type)
  "Return the union of the ranges RANGE1 and RANGE2.
If the ranges have conflicting types, use RANGE1's type.
This can be overridden with TYPE."
  (when (and (evil-range-p range1)
             (evil-range-p range2))
    (evil-range (min (evil-range-beginning range1)
                     (evil-range-beginning range2))
                (max (evil-range-end range1)
                     (evil-range-end range2))
                (or type
                    (evil-type range1)
                    (evil-type range2)))))

(defun evil-subrange-p (range1 range2)
  "Whether RANGE1 is contained within RANGE2."
  (and (evil-range-p range1)
       (evil-range-p range2)
       (<= (evil-range-beginning range2)
           (evil-range-beginning range1))
       (>= (evil-range-end range2)
           (evil-range-end range1))))

(defun evil-expand (beg end type &rest properties)
  "Expand BEG and END as TYPE with PROPERTIES.
Returns a list (BEG END TYPE PROPERTIES ...), where the tail
may contain a property list.

The overlay equivalent is `evil-expand-overlay'."
  (apply 'evil-transform
         ;; don't expand if already expanded
         (unless (plist-get properties :expanded) :expand)
         beg end type properties))

(defun evil-contract (beg end type &rest properties)
  "Contract BEG and END as TYPE with PROPERTIES.
Returns a list (BEG END TYPE PROPERTIES ...), where the tail
may contain a property list.

The overlay equivalent is `evil-contract-overlay'."
  (apply 'evil-transform 'contract beg end type properties))

(defun evil-normalize (beg end type &rest properties)
  "Normalize BEG and END as TYPE with PROPERTIES.
Returns a list (BEG END TYPE PROPERTIES ...), where the tail
may contain a property list.

The overlay equivalent is `evil-normalize-overlay'."
  (apply 'evil-transform 'normalize beg end type properties))

(defun evil-transform
  (transform beg end type &rest properties)
  "Apply TRANSFORM on BEG and END with PROPERTIES.
Returns a list (BEG END TYPE PROPERTIES ...), where the tail
may contain a property list. If TRANSFORM is undefined,
return positions unchanged.

The overlay equivalent is `evil-transform-overlay'."
  (let* ((type (or type (evil-type properties)))
         (transform (when (and type transform)
                      (evil-type-property type transform))))
    (if transform
        (apply transform beg end properties)
      (apply 'evil-range beg end type properties))))

(defun evil-describe (beg end type &rest properties)
  "Return description of BEG and END with PROPERTIES.
If no description is available, return the empty string.

The overlay equivalent is `evil-describe-overlay'."
  (let* ((type (or type (evil-type properties)))
         (properties (plist-put properties :type type))
         (describe (evil-type-property type :string)))
    (or (when describe
          (apply describe beg end properties))
        "")))

(defun evil-expand-range (range &optional copy)
  "Expand RANGE according to its type.
Return a new range if COPY is non-nil."
  (when copy
    (setq range (evil-copy-range range)))
  (unless (plist-get (evil-range-properties range) :expanded)
    (setq range (evil-transform-range :expand range)))
  range)

(defun evil-contract-range (range &optional copy)
  "Contract RANGE according to its type.
Return a new range if COPY is non-nil."
  (evil-transform-range 'contract range copy))

(defun evil-normalize-range (range &optional copy)
  "Normalize RANGE according to its type.
Return a new range if COPY is non-nil."
  (evil-transform-range 'normalize range copy))

(defun evil-transform-range (transform range &optional copy)
  "Apply TRANSFORM to RANGE according to its type.
Return a new range if COPY is non-nil."
  (when copy
    (setq range (evil-copy-range range)))
  (when (evil-type range)
    (apply 'evil-set-range range
           (apply 'evil-transform transform range)))
  range)

(defun evil-describe-range (range)
  "Return description of RANGE.
If no description is available, return the empty string."
  (apply 'evil-describe range))

(defun evil-expand-overlay (overlay &optional copy)
  "Expand OVERLAY according to its `type' property.
Return a new overlay if COPY is non-nil."
  (let ((type (evil-type overlay)))
    (when copy
      (setq overlay (copy-overlay overlay)))
    (unless (overlay-get overlay :expanded)
      (when (evil-type-property type :expand)
        ;; explicitly set :expanded to nil before expanding,
        ;; so that it is guaranteed to change back to nil
        ;; if the overlay is restored
        (overlay-put overlay :expanded nil)
        (setq overlay (evil-backup-overlay overlay)
              overlay (evil-transform-overlay :expand overlay))))
    overlay))

(defun evil-contract-overlay (overlay &optional copy)
  "Contract OVERLAY according to its `type' property.
If the type isn't injective, restore original positions.
Return a new overlay if COPY is non-nil."
  (let ((type (evil-type overlay)))
    (if (and type (evil-type-property type :injective))
        (setq overlay (evil-reset-overlay overlay copy)
              overlay (evil-transform-overlay 'contract overlay))
      (setq overlay (evil-restore-overlay overlay copy)))
    overlay))

(defun evil-normalize-overlay (overlay &optional copy)
  "Normalize OVERLAY according to its `type' property.
Return a new overlay if COPY is non-nil."
  (evil-transform-overlay 'normalize overlay copy))

(defun evil-transform-overlay (transform overlay &optional copy)
  "Apply TRANSFORM to OVERLAY according to its `type' property.
Return a new overlay if COPY is non-nil."
  (let* ((beg (overlay-start overlay))
         (end (overlay-end overlay))
         (type (evil-type overlay))
         (buffer (overlay-buffer overlay))
         (properties (overlay-properties overlay))
         (range (save-excursion
                  (with-current-buffer (or buffer (current-buffer))
                    (apply 'evil-transform
                           transform beg end type properties))))
         (beg (pop range))
         (end (pop range))
         (type (if (evil-type-p (car-safe range)) (pop range) type)))
    (when copy
      (setq overlay (copy-overlay overlay)))
    (while range
      (overlay-put overlay (pop range) (pop range)))
    (evil-set-type overlay type)
    (move-overlay overlay beg end buffer)
    overlay))

(defun evil-backup-overlay (overlay &optional copy)
  "Back up current OVERLAY positions and properties.
The information is stored in a :backup property.
Return a new overlay if COPY is non-nil."
  (let* ((beg (overlay-start overlay))
         (end (overlay-end overlay))
         (buffer (overlay-buffer overlay))
         (beg-marker (move-marker (make-marker) beg buffer))
         (end-marker (move-marker (make-marker) end buffer))
         (properties (overlay-properties overlay)))
    (setq overlay (evil-reset-overlay overlay copy))
    (set-marker-insertion-type beg-marker t)
    (set-marker-insertion-type end-marker nil)
    (overlay-put overlay :backup
                 (append (list beg-marker end-marker) properties))
    overlay))

(defun evil-restore-overlay (overlay &optional copy)
  "Restore previous OVERLAY positions and properties.
The information is retrieved from the :backup property.
Return a new overlay if COPY is non-nil."
  (let ((backup (overlay-get overlay :backup))
        beg end beg-marker end-marker properties buffer)
    (when copy
      (setq overlay (copy-overlay overlay)))
    (when backup
      (setq beg-marker (pop backup)
            end-marker (pop backup)
            properties backup
            beg (or (marker-position beg-marker)
                    (overlay-start overlay))
            end (or (marker-position end-marker)
                    (overlay-end overlay))
            buffer (or (marker-buffer beg-marker)
                       (marker-buffer end-marker)
                       (overlay-buffer overlay))
            overlay (evil-reset-overlay overlay copy))
      (move-overlay overlay beg end buffer)
      (while properties
        (overlay-put overlay (pop properties) (pop properties))))
    overlay))

(defun evil-reset-overlay (overlay &optional copy)
  "Reset back-up information for OVERLAY.
Return a new overlay if COPY is non-nil."
  (let* ((backup (overlay-get overlay :backup))
         (beg (pop backup))
         (end (pop backup)))
    (cond
     (copy
      (setq overlay (copy-overlay overlay)))
     ;; unless we're making a copy, delete old markers
     ;; so they don't slow down editing
     (backup
      (set-marker beg nil)
      (set-marker end nil)))
    (overlay-put overlay :backup nil)
    overlay))

(defun evil-describe-overlay (overlay)
  "Return description of OVERLAY.
If no description is available, return the empty string."
  (let ((beg (overlay-start overlay))
        (end (overlay-end overlay))
        (type (evil-type overlay))
        (buffer (overlay-buffer overlay))
        (properties (overlay-properties overlay)))
    (save-excursion
      (with-current-buffer (or buffer (current-buffer))
        (apply 'evil-describe
               beg end type properties)))))

(defmacro evil-define-type (type doc &rest body)
  "Define type TYPE.
DOC is a general description and shows up in all docstrings.
It is followed by a list of keywords and functions:

:expand FUNC    Expansion function. This function should accept
                two positions in the current buffer, BEG and END,
                and return a pair of expanded buffer positions.
:contract FUNC  The opposite of :expand, optional.
:injective BOOL Whether expansion is one-to-one. This means that
                :expand followed by :contract always returns the
                original range.
:normalize FUNC Normalization function, optional. This function should
                accept two unexpanded positions and adjust them before
                expansion. May be used to deal with buffer boundaries.
:string FUNC    Description function. This takes two buffer positions
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
    (setq plist (plist-put plist :injective t))
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
              (let (type range)
                (when (and beg end)
                  (save-excursion
                    (evil-sort beg end)
                    (unless (plist-get properties :expanded)
                      (setq range (evil-expand
                                   beg end ',type properties)
                            beg  (or (pop range) beg)
                            end  (or (pop range) end)
                            type (if (evil-type-p (car-safe range))
                                     (pop range) type))
                      (while range
                        (setq properties
                              (plist-put properties
                                         (pop range) (pop range)))))
                    (or (apply ',func beg end
                               (when ,(> (length args) 2)
                                 properties))
                        ""))))))
          (t
           `(defun ,name (beg end &rest properties)
              ,(format "Perform %s transformation on %s from BEG to END \
with PROPERTIES.\n\n%s%s" sym type string doc)
              (let ((type ',type) range)
                (when (and beg end)
                  (save-excursion
                    (when (memq ,key '(:expand :contract))
                      (setq properties
                            (plist-put properties
                                       :expanded
                                       ,(eq key :expand))))
                    (setq range (apply 'evil-range beg end type properties)
                          beg (car range)
                          end (cadr range))
                    (setq range (or (apply ',func beg end
                                           (when ,(> (length args) 2)
                                             properties))
                                    range)
                          beg  (or (pop range) beg)
                          end  (or (pop range) end)
                          type (if (evil-type-p (car-safe range))
                                   (pop range) type))
                    (while range
                      (setq properties
                            (plist-put properties
                                       (pop range) (pop range))))
                    (apply 'evil-range beg end type properties)))))))
         t)))
    ;; :injective presupposes both or neither of :expand and :contract
    (when (plist-get plist :expand)
      (setq plist (plist-put plist :injective
                             (and (plist-get plist :contract)
                                  (plist-get plist :injective)))))
    `(progn
       (evil-put-property 'evil-type-properties ',type ,@plist)
       ,@defun-forms
       ',type)))

;;; Type definitions

(evil-define-type exclusive
  "Return the positions unchanged, with some exceptions.
If the end position is at the beginning of a line, then:

* If the beginning position is at or before the first non-blank
  character on the line, return `line' (expanded).

* Otherwise, move the end position to the end of the previous
  line and return `inclusive' (expanded)."
  :normalize (lambda (beg end)
               (cond
                ((progn
                   (goto-char end)
                   (and (/= beg end) (bolp)))
                 (setq end (max beg (1- end)))
                 (cond
                  ((progn
                     (goto-char beg)
                     (looking-back "^[ \f\t\v]*"))
                   (evil-expand beg end 'line))
                  (t
                   (unless evil-cross-lines
                     (setq end (max beg (1- end))))
                   (evil-expand beg end 'inclusive))))
                (t
                 (evil-range beg end))))
  :string (lambda (beg end)
            (let ((width (- end beg)))
              (format "%s character%s" width
                      (if (= width 1) "" "s")))))

(evil-define-type inclusive
  "Include the character under point."
  :expand (lambda (beg end)
            (evil-range beg (1+ end)))
  :contract (lambda (beg end)
              (evil-range beg (max beg (1- end))))
  :normalize (lambda (beg end)
               (goto-char end)
               (when (eq (char-after) ?\n)
                 (setq end (max beg (1- end))))
               (evil-range beg end))
  :string (lambda (beg end)
            (let ((width (- end beg)))
              (format "%s character%s" width
                      (if (= width 1) "" "s")))))

(evil-define-type line
  "Include whole lines."
  :injective nil
  :expand (lambda (beg end)
            (evil-range
             (progn
               (goto-char beg)
               (line-beginning-position))
             (progn
               (goto-char end)
               (line-beginning-position 2))))
  :contract (lambda (beg end)
              (evil-range beg (max beg (1- end))))
  :string (lambda (beg end)
            (let ((height (count-lines beg end)))
              (format "%s line%s" height
                      (if (= height 1) "" "s")))))

(evil-define-type block
  "Like `inclusive', but for rectangles:
the last column is included."
  :expand (lambda (beg end &rest properties)
            (let* ((beg-col (progn
                              (goto-char beg)
                              (current-column)))
                   (end-col (progn
                              (goto-char end)
                              (current-column)))
                   (corner (plist-get properties :corner)))
              ;; Because blocks are implemented as a pair of buffer
              ;; positions, expansion is restricted to what the buffer
              ;; allows. In the case of a one-column block, there are
              ;; two ways to expand it (either increase the upper
              ;; corner beyond the lower corner, or increase the lower
              ;; beyond the upper), so we try out both possibilities
              ;; when we encounter the end of the line.
              (cond
               ((= beg-col end-col)
                (goto-char end)
                (cond
                 ((eolp)
                  (goto-char beg)
                  (if (eolp)
                      (evil-range beg end)
                    (evil-range (1+ beg) end)))
                 ((memq corner '(lower-right upper-right right))
                  (evil-range (1+ beg) end))
                 (t
                  (evil-range beg (1+ end)))))
               ((< beg-col end-col)
                (goto-char end)
                (if (eolp)
                    (evil-range beg end)
                  (evil-range beg (1+ end))))
               (t
                (goto-char beg)
                (if (eolp)
                    (evil-range beg end)
                  (evil-range (1+ beg) end))))))
  :contract (lambda (beg end)
              (let* ((beg-col (progn
                                (goto-char beg)
                                (current-column)))
                     (end-col (progn
                                (goto-char end)
                                (current-column))))
                (if (> beg-col end-col)
                    (evil-range (1- beg) end)
                  (evil-range beg (max beg (1- end))))))
  :string (lambda (beg end)
            (let ((height (count-lines
                           beg
                           (progn
                             (goto-char end)
                             (if (and (bolp) (not (eobp)))
                                 (1+ end)
                               end))))
                  (width (abs (- (progn
                                   (goto-char beg)
                                   (current-column))
                                 (progn
                                   (goto-char end)
                                   (current-column))))))
              (format "%s row%s and %s column%s"
                      height
                      (if (= height 1) "" "s")
                      width
                      (if (= width 1) "" "s"))))
  :rotate (lambda (beg end &rest properties)
            "Rotate block according to :corner property.
:corner can be one of `upper-left',``upper-right', `lower-left'
and `lower-right'."
            (let* ((left (progn
                           (goto-char beg)
                           (current-column)))
                   (right (progn
                            (goto-char end)
                            (current-column)))
                   (corner (or (plist-get properties :corner)
                               'upper-left)))
              (evil-sort left right)
              (goto-char beg)
              (if (memq corner '(upper-right lower-left))
                  (move-to-column right)
                (move-to-column left))
              (setq beg (point))
              (goto-char end)
              (if (memq corner '(upper-right lower-left))
                  (move-to-column left)
                (move-to-column right))
              (setq end (point))
              (setq properties (plist-put properties
                                          :corner corner))
              (apply 'evil-range beg end properties))))

(provide 'evil-types)

;;; evil-types.el ends here
