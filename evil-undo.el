;;;; Undo

(require 'evil-common)

;; load undo-tree.el if available
(unless (featurep 'undo-tree)
  (condition-case nil
      (require 'undo-tree)
    (error nil)))

(when (fboundp 'global-undo-tree-mode)
  (global-undo-tree-mode 1))

(defmacro evil-single-undo (&rest body)
  "Execute BODY as a single undo step."
  (declare (indent 0)
           (debug t))
  `(unwind-protect
       (progn
         (evil-start-undo-step)
         ,@body)
     (evil-end-undo-step)))

(defun evil-start-undo-step ()
  "Start a undo step.
All following buffer modifications are grouped together
as a single action. The step is terminated with `evil-end-undo-step'
or by exiting to Normal state."
  (when (listp buffer-undo-list)
    (unless (null (car-safe buffer-undo-list))
      (add-to-list 'buffer-undo-list nil))
    (setq evil-undo-list-pointer buffer-undo-list)
    ;; continually refresh the undo entries for the step,
    ;; ensuring proper synchronization between `buffer-undo-list'
    ;; and undo-tree.el's `buffer-undo-tree'
    (add-hook 'post-command-hook 'evil-refresh-undo-step nil t)))

(defun evil-end-undo-step ()
  "End a undo step started with `evil-start-undo-step'."
  (when (memq 'evil-refresh-undo-step post-command-hook)
    (evil-refresh-undo-step)
    (undo-boundary)
    (remove-hook 'post-command-hook 'evil-refresh-undo-step t)))

;; TODO: This destroys undo information during an undo step.
;; Ideally, we'd like to postpone it until the step is ended,
;; so that "C-_" works reliably in Insert state.
(defun evil-refresh-undo-step ()
  "Refresh `buffer-undo-list' entries for current undo step.
Undo boundaries until `evil-undo-list-pointer' are removed
to make the entries undoable as a single action.
See `evil-start-undo-step'."
  (setq buffer-undo-list
        (evil-filter-list 'null buffer-undo-list
                          evil-undo-list-pointer)
        evil-undo-list-pointer buffer-undo-list))

;;; Undo ring

(defmacro evil-with-undo (&rest body)
  "Executes the body with enabled undo. If undo is disabled in
the current buffer, the undo information is stored in
`evil-temporary-undo' instead of `buffer-undo-list'."
  (declare (indent defun)
           (debug t))
  `(let ((orig-buffer-undo-list t))
     (let (buffer-undo-list)
       ,@body
       (setq evil-temporary-undo (cons nil buffer-undo-list)))
     (unless (eq buffer-undo-list t)
       ;; undo is enabled, so update the global buffer undo list
       (setq buffer-undo-list (append evil-temporary-undo buffer-undo-list)
             evil-temporary-undo nil))))

(defun evil-undo-pop ()
  "Undos the last buffer change and removes the last undo
information from `buffer-undo-list'. If undo is disabled in the
current buffer, use the information in `evil-temporary-undo'
instead."
  (let ((paste-undo (list nil)))
    (let ((undo-list (if (eq buffer-undo-list t)
                         evil-temporary-undo
                       buffer-undo-list)))
      (when (or (not undo-list) (car undo-list))
        (error "Can't undo previous paste"))
      (pop undo-list) ; remove nil
      (while (and undo-list
                  (car undo-list))
        (push (pop undo-list) paste-undo))
      (let ((buffer-undo-list (nreverse paste-undo))
            (orig-message (symbol-function 'message)))
        (fset 'message #'(lambda (&rest rest)))
        (undo)
        (fset 'message orig-message))
      (if (eq buffer-undo-list t)
          (setq evil-temporary-undo nil)
        (setq buffer-undo-list undo-list)))))

(provide 'evil-undo)

;;; evil-undo.el ends here
