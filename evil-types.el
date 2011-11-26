;;;; Type system

;; A type defines a transformation on a pair of buffer positions.
;; Types are used by Visual state (character/line/block selection)
;; and Operator-Pending state (character/line/block motions).
;;
;; The basic transformation is "expansion". For example, the `line'
;; type "expands" a pair of positions to whole lines by moving the
;; first position to the beginning of the line and the last position
;; to the end of the line. That expanded selection is what the rest
;; of Emacs sees and acts on.
;;
;; An optional transformation is "contraction", which is the opposite
;; of expansion. If the transformation is one-to-one, expansion
;; followed by contraction always returns the original range.
;; (The `line' type is not one-to-one, as it may expand multiple
;; positions to the same lines.)
;;
;; Another optional transformation is "normalization", which takes
;; two unexpanded positions and adjusts them before expansion.
;; This is useful for cleaning up "invalid" positions.
;;
;; Types are defined at the end of this file using the macro
;; `evil-define-type'.

(require 'evil-common)
(require 'evil-macros)

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
  :one-to-one nil
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
              ;; Since blocks are implemented as a pair of buffer
              ;; positions, expansion is restricted to what the buffer
              ;; allows. In the case of a one-column block, there are
              ;; two ways to expand it (either move the upper corner
              ;; beyond the lower corner, or the lower beyond the
              ;; upper), so try out both possibilities when
              ;; encountering the end of the line.
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

;;; Standard interactive codes

(evil-define-interactive-code "*"
  "Signal error if the buffer is read-only."
  (when buffer-read-only
    (signal 'buffer-read-only nil)))

(evil-define-interactive-code "b" (prompt)
  "Name of existing buffer."
  (list (read-buffer prompt (current-buffer) t)))

(evil-define-interactive-code "c"
  "Read character."
  (list (read-char)))

(evil-define-interactive-code "p"
  "Prefix argument converted to number."
  (list (prefix-numeric-value current-prefix-arg)))

(evil-define-interactive-code "P"
  "Prefix argument in raw form."
  (list current-prefix-arg))

;;; Custom interactive codes

(evil-define-interactive-code "<c>"
  "Count."
  (list (when current-prefix-arg
          (prefix-numeric-value
           current-prefix-arg))))

(evil-define-interactive-code "<r>"
  "Untyped motion range (BEG END)."
  (evil-operator-range))

(evil-define-interactive-code "<R>"
  "Typed motion range (BEG END TYPE)."
  (evil-operator-range t))

(evil-define-interactive-code "<x>"
  "Current register."
  (list evil-this-register))

(evil-define-interactive-code "<y>"
  "Current yank-handler."
  (list (evil-yank-handler)))

(evil-define-interactive-code "<f>"
  :ex-arg file
  (list (and (evil-ex-state-p) (evil-ex-file-arg))))

(evil-define-interactive-code "<b>"
  :ex-arg buffer
  (list (and (evil-ex-state-p) evil-ex-current-arg)))

(evil-define-interactive-code "<a>"
  :ex-arg t
  (list (and (evil-ex-state-p) evil-ex-current-arg)))

(evil-define-interactive-code "<!>"
  :ex-force t
  (list (and (evil-ex-state-p) evil-ex-current-cmd-force)))

(evil-define-interactive-code "<sym>"
  :ex-arg sym
  (list (and (evil-ex-state-p)
             evil-ex-current-arg
             (intern evil-ex-current-arg))))

(evil-define-interactive-code "<s/>"
  :ex-arg substitution
  (list (and (evil-ex-state-p) evil-ex-current-arg)))

(provide 'evil-types)

;;; evil-types.el ends here
