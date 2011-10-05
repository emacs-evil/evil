;;;; Undo

(require 'evil-common)

;; load undo-tree.el if available
(unless (featurep 'undo-tree)
  (condition-case nil
      (require 'undo-tree)
    (error nil)))

(when (fboundp 'global-undo-tree-mode)
  (global-undo-tree-mode 1))

(defmacro evil-with-single-undo (&rest body)
  "Execute BODY as a single undo step."
  (declare (indent defun)
           (debug t))
  `(let (evil-undo-list-pointer)
     (evil-with-undo
       (unwind-protect
           (progn
             (evil-start-undo-step)
             ,@body)
         (evil-end-undo-step)))))

(defun evil-start-undo-step (&optional continue)
  "Start a undo step.
All following buffer modifications are grouped together as a
single action. If CONTINUE is non-nil, preceding modifications
are included. The step is terminated with `evil-end-undo-step'."
  (when (listp buffer-undo-list)
    (if evil-undo-list-pointer
        (evil-refresh-undo-step)
      (unless (or continue (null (car-safe buffer-undo-list)))
        (undo-boundary))
      (setq evil-undo-list-pointer (or buffer-undo-list t)))))

(defun evil-end-undo-step (&optional continue)
  "End a undo step started with `evil-start-undo-step'.
Adds an undo boundary unless CONTINUE is specified."
  (when evil-undo-list-pointer
    (evil-refresh-undo-step)
    (unless continue
      (undo-boundary))
    (remove-hook 'post-command-hook 'evil-refresh-undo-step t)
    (setq evil-undo-list-pointer nil)))

;; TODO: This destroys undo information during an undo step.
;; Ideally, we'd like to postpone it until the step is ended,
;; so that "C-_" works properly in Insert state.
(defun evil-refresh-undo-step ()
  "Refresh `buffer-undo-list' entries for current undo step.
Undo boundaries until `evil-undo-list-pointer' are removed
to make the entries undoable as a single action.
See `evil-start-undo-step'."
  (when evil-undo-list-pointer
    (setq buffer-undo-list
          (evil-filter-list 'null buffer-undo-list
                            evil-undo-list-pointer)
          evil-undo-list-pointer (or buffer-undo-list t))))

;;; Undo ring

(defmacro evil-with-undo (&rest body)
  "Execute BODY with enabled undo.
If undo is disabled in the current buffer, the undo information
is stored in `evil-temporary-undo' instead of `buffer-undo-list'."
  (declare (indent defun)
           (debug t))
  `(unwind-protect
       (let (buffer-undo-list)
         ,@body
         (setq evil-temporary-undo (cons nil buffer-undo-list)))
     (unless (eq buffer-undo-list t)
       ;; undo is enabled, so update the global buffer undo list
       (setq buffer-undo-list
             ;; prepend new undos (if there are some)
             (if (cdr evil-temporary-undo)
                 (nconc evil-temporary-undo buffer-undo-list)
               buffer-undo-list)
             evil-temporary-undo nil))))

(defun evil-undo-pop ()
  "Undo the last buffer change.
Removes the last undo information from `buffer-undo-list'.
If undo is disabled in the current buffer, use the information
in `evil-temporary-undo' instead."
  (let ((paste-undo (list nil)))
    (let ((undo-list (if (eq buffer-undo-list t)
                         evil-temporary-undo
                       buffer-undo-list)))
      (when (or (not undo-list) (car undo-list))
        (error "Can't undo previous change"))
      (while (and undo-list (null (car undo-list)))
        (pop undo-list)) ; remove nil
      (while (and undo-list (car undo-list))
        (push (pop undo-list) paste-undo))
      (let ((buffer-undo-list (nreverse paste-undo)))
        (evil-save-echo-area
          (undo)))
      (if (eq buffer-undo-list t)
          (setq evil-temporary-undo nil)
        (setq buffer-undo-list undo-list)))))

(provide 'evil-undo)

;;; evil-undo.el ends here
