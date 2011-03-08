;;;; Type system

;; A type defines a transformation on a pair of buffer positions.
;; Types are used by Visual state (character/line/block selection)
;; and Operator-Pending state (character/line/block motions).
;;
;; The basic transformation is "expansion". For example, the `line'
;; type "expands" a pair of positions to whole lines by moving the
;; first position to the beginning of its line and the last position
;; to the end of its line. That line selection is what the rest of
;; Emacs actually sees and acts on.
;;
;; The opposite of expansion is "contraction", which requires the
;; expansion to be one-to-one. The `inclusive' type, which increases
;; the last position by one, is one-to-one and contractable. The
;; `line' type has no contraction procedure, since it may expand
;; multiple positions to the same lines.
;;
;; Types are defined at the end of this file using the macro
;; `evil-define-type'.

(require 'evil-vars)
(require 'evil-common)

(defun evil-type (object &optional default)
  "Return the type of OBJECT, or DEFAULT if none."
  (or (cond
       ((overlayp object)
        (overlay-get object 'type))
       ((listp object)
        ;; (BEG END TYPE)
        (if (and (>= (length object) 3)
                 (numberp (nth 0 object))
                 (numberp (nth 1 object))
                 (symbolp (nth 2 object)))
            (nth 2 object)
          ;; property list
          (plist-get object 'type)))
       ;; motion
       ((symbolp object)
        (get object 'type)))
      default))

(defun evil-set-type (object type)
  "Set the type of OBJECT to TYPE.
For example, (evil-set-type 'next-line 'line)
will make `line' the type of the `next-line' command."
  (cond
   ((overlayp object)
    (overlay-put object 'type type))
   ((listp object)
    (if (and (>= (length object) 3)
             (numberp (nth 0 object))
             (numberp (nth 1 object))
             (symbolp (nth 2 object)))
        (setcar (nthcdr 2 object) type)
      (plist-put object 'type type)))
   ((symbolp object)
    (put object 'type type)))
  object)

(defun evil-expand (beg end type &optional buffer &rest properties)
  "Expand BEG and END as TYPE in BUFFER with PROPERTIES.
Returns a list (BEG END TYPE PROPERTIES ...), where the tail
may contain a property list."
  (apply 'evil-transform beg end type 'expand buffer properties))

(defun evil-contract (beg end type &optional buffer &rest properties)
  "Contract BEG and END as TYPE in BUFFER with PROPERTIES.
Returns a list (BEG END TYPE PROPERTIES ...), where the tail
may contain a property list."
  (apply 'evil-transform beg end type 'contract buffer properties))

(defun evil-transform
  (beg end type transform &optional buffer &rest properties)
  "Apply TRANSFORM on BEG and END in BUFFER with PROPERTIES.
Returns a list (BEG END TYPE PROPERTIES ...), where the tail
may contain a property list. If TRANSFORM is undefined,
return positions unchanged."
  (let* ((type (or type (evil-type properties)))
         (buffer (or buffer (current-buffer)))
         (transform (when (and type transform)
                      (evil-type-property type transform))))
    (if transform
        (apply transform beg end buffer properties)
      (setq beg (if (markerp beg) (marker-position beg) beg)
            end (if (markerp end) (marker-position end) end))
      (if type
          (append (list beg end type) properties)
        (append (list beg end) properties)))))

(defun evil-describe (beg end type &optional buffer &rest properties)
  "Return description of BEG and END in BUFFER with PROPERTIES.
If no description is available, return the empty string."
  (let* ((type (or type (evil-type properties)))
         (properties (plist-put properties 'type type))
         (describe (evil-type-property type :string)))
    (or (when describe
          (apply describe beg end buffer properties))
        "")))

(defun evil-type-property (type prop)
  "Return property PROP for TYPE."
  (evil-get-property evil-types-alist type prop))

(defmacro evil-define-type (type doc &rest body)
  "Define type TYPE.
DOC is a general description and shows up in all docstrings.
It is followed by a list of keywords and functions:

:expand FUNC    Expansion function. This function should take two
                positions in the current buffer, BEG and END, and
                return a pair of expanded buffer positions.
:contract FUNC  Contraction function, optional. This is the opposite
                of :expand, provided the expansion is reversible.
:string FUNC    Description function. This takes two buffer positions
                and returns a human-readable string.

Further keywords and functions may be specified. These are assumed to
be transformations on buffer positions, like :expand and :contract."
  (declare (debug (&define name
                           [&optional stringp]
                           [&rest [keywordp function-form]]))
           (indent defun))
  (let (args defun-forms func keyword name plist string sym)
    (while (keywordp (car body))
      (setq keyword (pop body)
            func (pop body)
            sym (intern (replace-regexp-in-string
                         "^:" "" (symbol-name keyword)))
            name (intern (format "evil-%s-%s" type sym))
            args (car (cdr-safe func))
            string (car (cdr (cdr-safe func)))
            string (if (stringp string) string "")
            plist (plist-put plist keyword `',name))
      (add-to-list
       'defun-forms
       (cond
        ((eq keyword :string)
         `(defun ,name (beg end &optional buffer &rest properties)
            ,(format "Return size of %s from BEG to END \
in BUFFER with PROPERTIES.\n%s\n\n%s" type string doc)
            (let ((buffer (or buffer (current-buffer))) type range)
              (when (and beg end)
                (save-excursion
                  (switch-to-buffer buffer)
                  (evil-sort beg end)
                  (unless (plist-get properties 'expanded)
                    (setq range (evil-expand
                                 beg end ',type buffer properties)
                          beg (pop range)
                          end (pop range))
                    (when (symbolp (car range))
                      (setq type (or (pop range) type)))
                    (while range
                      (setq properties
                            (plist-put properties
                                       (pop range) (pop range)))))
                  (or (apply ',func beg end nil) ""))))))
        (t
         `(defun ,name (beg end &optional buffer &rest properties)
            ,(format "Perform %s transformation on %s from BEG to END \
in BUFFER with PROPERTIES.\n%s\n\n%s" sym type string doc)
            (let ((buffer (or buffer (current-buffer)))
                  (type ',type) range)
              (when (and beg end)
                (save-excursion
                  (switch-to-buffer buffer)
                  (when (markerp beg)
                    (setq beg (marker-position beg)))
                  (when (markerp end)
                    (setq end (marker-position end)))
                  (evil-sort beg end)
                  ,@(when (memq keyword '(:expand :contract))
                      `((when (plist-get properties 'expanded)
                          (setq properties
                                (plist-put properties
                                           'expanded
                                           ,(eq keyword :expand))))))
                  (setq range (apply ',func beg end nil)
                        beg (pop range)
                        end (pop range))
                  (evil-sort beg end)
                  (when (symbolp (car range))
                    (setq type (or (pop range) type)))
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
  :expand (lambda (beg end)
            (cond
             ((and (/= beg end)
                   (progn
                     (goto-char end)
                     (bolp)))
              (backward-char)
              (setq end (max beg (point)))
              (cond
               ((progn
                  (goto-char beg)
                  (looking-back "^[ \f\t\v]*"))
                (evil-expand beg end 'line))
               (t
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
                   (corner (plist-get properties 'corner)))
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
            (let* ((beg-col (progn
                              (goto-char beg)
                              (current-column)))
                   (end-col (progn
                              (goto-char end)
                              (current-column)))
                   (left  (min beg-col end-col))
                   (right (max beg-col end-col))
                   (corner (or (plist-get properties 'corner)
                               'upper-left)))
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
                                          'corner corner))
              (append (list beg end) properties))))

(provide 'evil-types)

;;; evil-types.el ends here
