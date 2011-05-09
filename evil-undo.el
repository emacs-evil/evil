;;;; Undo

(require 'evil-vars)
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
        (evil-filter-list buffer-undo-list 'null
                          evil-undo-list-pointer)))

;;; Undo tree visualizer

(add-to-list 'evil-motion-state-modes 'undo-tree-visualizer-mode)

(when (boundp 'undo-tree-visualizer-map)
  (define-key undo-tree-visualizer-map [remap evil-backward-char] 'undo-tree-visualize-switch-branch-left)
  (define-key undo-tree-visualizer-map [remap evil-forward-char] 'undo-tree-visualize-switch-branch-right)
  (define-key undo-tree-visualizer-map [remap evil-next-line] 'undo-tree-visualize-redo)
  (define-key undo-tree-visualizer-map [remap evil-previous-line] 'undo-tree-visualize-undo))

(provide 'evil-undo)

;;; evil-undo.el ends here
