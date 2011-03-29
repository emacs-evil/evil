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

(require 'evil-vars)
(require 'evil-common)

(defun evil-type (object &optional default)
  "Return the type of OBJECT, or DEFAULT if none."
  (or (cond
       ((overlayp object)
        (overlay-get object :type))
       ((listp object)
        ;; (BEG END TYPE)
        (if (and (>= (length object) 3)
                 (numberp (nth 0 object))
                 (numberp (nth 1 object))
                 (symbolp (nth 2 object)))
            (nth 2 object)
          ;; property list
          (plist-get object :type)))
       ;; command
       ((commandp object)
        (evil-get-command-property object :type))
       ((symbolp object)
        (get object 'type)))
      default))

(defun evil-set-type (object type)
  "Set the type of OBJECT to TYPE.
For example, (evil-set-type 'next-line 'line)
will make `line' the type of the `next-line' command."
  (cond
   ((overlayp object)
    (overlay-put object :type type))
   ((listp object)
    (if (and (>= (length object) 3)
             (numberp (nth 0 object))
             (numberp (nth 1 object))
             (symbolp (nth 2 object)))
        (setcar (nthcdr 2 object) type)
      (plist-put object :type type)))
   ((commandp object)
    (evil-add-command-properties object :type type))
   ((symbolp object)
    (put object 'type type)))
  object)

(defun evil-expand (beg end type &rest properties)
  "Expand BEG and END as TYPE with PROPERTIES.
Returns a list (BEG END TYPE PROPERTIES ...), where the tail
may contain a property list.

The overlay equivalent is `evil-expand-overlay'."
  (apply 'evil-transform beg end type
         ;; don't expand if already expanded
         (unless (plist-get properties :expanded) 'expand)
         properties))

(defun evil-contract (beg end type &rest properties)
  "Contract BEG and END as TYPE with PROPERTIES.
Returns a list (BEG END TYPE PROPERTIES ...), where the tail
may contain a property list.

The overlay equivalent is `evil-contract-overlay'."
  (apply 'evil-transform beg end type 'contract properties))

(defun evil-normalize (beg end type &rest properties)
  "Normalize BEG and END as TYPE with PROPERTIES.
Returns a list (BEG END TYPE PROPERTIES ...), where the tail
may contain a property list.

The overlay equivalent is `evil-normalize-overlay'."
  (apply 'evil-transform beg end type 'normalize properties))

(defun evil-transform
  (beg end type transform &rest properties)
  "Apply TRANSFORM on BEG and END with PROPERTIES.
Returns a list (BEG END TYPE PROPERTIES ...), where the tail
may contain a property list. If TRANSFORM is undefined,
return positions unchanged.

The overlay equivalent is `evil-transform-overlay'."
  (let* ((beg (if (markerp beg) (marker-position beg) beg))
         (end (if (markerp end) (marker-position end) end))
         (type (or type (evil-type properties)))
         (transform (when (and type transform)
                      (evil-type-property type transform))))
    (cond
     (transform
      (apply transform beg end properties))
     (type
      (append (list beg end type) properties))
     (t
      (append (list beg end) properties)))))

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

(defun evil-expand-overlay (overlay &optional copy)
  "Expand OVERLAY according to its `type' property.
Return a new overlay if COPY is non-nil."
  (let ((type (evil-type overlay)))
    (when copy
      (setq overlay (copy-overlay overlay)))
    (unless (overlay-get overlay :expanded)
      (when (and type (evil-type-property type 'expand))
        ;; explicitly set :expanded to nil before expanding,
        ;; so that it is guaranteed to change back to nil
        ;; if the overlay is restored
        (overlay-put overlay :expanded nil)
        (setq overlay (evil-backup-overlay overlay)
              overlay (evil-transform-overlay overlay 'expand))))
    overlay))

(defun evil-contract-overlay (overlay &optional copy)
  "Contract OVERLAY according to its `type' property.
If the type cannot be contracted, restore original positions.
Return a new overlay if COPY is non-nil."
  (let ((type (evil-type overlay)))
    (if (and type (evil-type-property type 'contract))
        (setq overlay (evil-reset-overlay overlay copy)
              overlay (evil-transform-overlay overlay 'contract))
      (setq overlay (evil-restore-overlay overlay copy)))
    overlay))

(defun evil-normalize-overlay (overlay &optional copy)
  "Normalize OVERLAY according to its `type' property.
Return a new overlay if COPY is non-nil."
  (evil-transform-overlay overlay 'normalize copy))

(defun evil-transform-overlay (overlay transform &optional copy)
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
                           beg end type transform properties))))
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
The information is stored in a `backup' property.
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
    (overlay-put overlay 'backup
                 (append (list beg-marker end-marker) properties))
    overlay))

(defun evil-restore-overlay (overlay &optional copy)
  "Restore previous OVERLAY positions and properties.
The information is retrieved from the `backup' property.
Return a new overlay if COPY is non-nil."
  (let ((backup (overlay-get overlay 'backup))
        beg end beg-marker end-marker properties buffer)
    (cond
     (backup
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
     (copy
      (setq overlay (copy-overlay overlay))))
    overlay))

(defun evil-reset-overlay (overlay &optional copy)
  "Reset back-up information for OVERLAY.
Return a new overlay if COPY is non-nil."
  (let* ((backup (overlay-get overlay 'backup))
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
    (overlay-put overlay 'backup nil)
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

(defun evil-type-property (type prop)
  "Return property PROP for TYPE."
  (evil-get-property evil-types-alist type prop))

(defun evil-type-p (sym)
  "Whether SYM is the name of a type."
  (assq sym evil-types-alist))

(defmacro evil-define-type (type doc &rest body)
  "Define type TYPE.
DOC is a general description and shows up in all docstrings.
It is followed by a list of keywords and functions:

:expand FUNC    Expansion function. This function should accept
                two positions in the current buffer, BEG and END,
                and return a pair of expanded buffer positions.
:contract FUNC  Contraction function, optional. This is the opposite
                of :expand (provided the expansion is reversible).
:normalize FUNC Normalization function, optional. This function should
                accept two unexpanded positions and adjust them before
                expansion.
:string FUNC    Description function. This takes two buffer positions
                and returns a human-readable string.

Further keywords and functions may be specified. These are assumed to
be transformations on buffer positions, like :expand and :contract."
  (declare (debug (&define name
                           [&optional stringp]
                           [&rest [keywordp function-form]]))
           (indent defun))
  (let (args defun-forms func keyword name plist string sym)
    (while (keywordp (car-safe body))
      (setq keyword (pop body)
            func (pop body)
            sym (intern (replace-regexp-in-string
                         "^:" "" (symbol-name keyword)))
            name (intern (format "evil-%s-%s" type sym))
            args (car (cdr-safe func))
            string (car (cdr (cdr-safe func)))
            string (if (stringp string)
                       (format "%s\n\n" string) "")
            plist (plist-put plist keyword `',name))
      (add-to-list
       'defun-forms
       (cond
        ((eq keyword :string)
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
                  (when (markerp beg)
                    (setq beg (marker-position beg)))
                  (when (markerp end)
                    (setq end (marker-position end)))
                  (evil-sort beg end)
                  (when (memq ,keyword '(:expand :contract))
                    (setq properties
                          (plist-put properties
                                     :expanded
                                     ,(eq keyword :expand))))
                  (setq range (or (apply ',func beg end
                                         (when ,(> (length args) 2)
                                           properties))
                                  (append (list beg end type)
                                          properties))
                        beg  (or (pop range) beg)
                        end  (or (pop range) end)
                        type (if (evil-type-p (car-safe range))
                                 (pop range) type))
                  (evil-sort beg end)
                  (while range
                    (setq properties
                          (plist-put properties
                                     (pop range) (pop range))))
                  (append (list beg end type) properties))))))) t))
    `(progn
       (evil-put-property 'evil-types-alist ',type ,@plist)
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
                   (setq end (max beg (1- end)))
                   (evil-expand beg end 'inclusive))))
                (t
                 (list beg end))))
  :string (lambda (beg end)
            (let ((width (- end beg)))
              (format "%s character%s" width
                      (if (= width 1) "" "s")))))

(evil-define-type inclusive
  "Include the character under point."
  :expand (lambda (beg end)
            (list beg (min (1+ end) (point-max))))
  :contract (lambda (beg end)
              (list beg (max beg (1- end))))
  :normalize (lambda (beg end)
               (goto-char end)
               (when (eq (char-after) ?\n)
                 (setq end (max beg (1- end))))
               (list beg end))
  :string (lambda (beg end)
            (let ((width (- end beg)))
              (format "%s character%s" width
                      (if (= width 1) "" "s")))))

(evil-define-type line
  "Include whole lines."
  :expand (lambda (beg end)
            (list
             (progn
               (goto-char beg)
               (line-beginning-position))
             (progn
               (goto-char end)
               (line-beginning-position 2))))
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
              (cond
               ((= beg-col end-col)
                (goto-char end)
                (cond
                 ((eolp)
                  (goto-char beg)
                  (if (eolp)
                      (list beg end)
                    (list (1+ beg) end)))
                 ((memq corner '(lower-right upper-right right))
                  (list (1+ beg) end))
                 (t
                  (list beg (1+ end)))))
               ((< beg-col end-col)
                (goto-char end)
                (if (eolp)
                    (list beg end)
                  (list beg (1+ end))))
               (t
                (goto-char beg)
                (if (eolp)
                    (list beg end)
                  (list (1+ beg) end))))))
  :contract (lambda (beg end)
              (let* ((beg-col (progn
                                (goto-char beg)
                                (current-column)))
                     (end-col (progn
                                (goto-char end)
                                (current-column))))
                (if (> beg-col end-col)
                    (list (1- beg) end)
                  (list beg (max beg (1- end))))))
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
              (append (list beg end) properties))))

(provide 'evil-types)

;;; evil-types.el ends here
