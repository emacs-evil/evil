;;;; Integrate Evil with other modules

(require 'evil-states)
(require 'evil-motions)

;;; ERT

(add-to-list 'evil-emacs-state-modes 'ert-results-mode)

;;; Undo tree visualizer

(add-to-list 'evil-motion-state-modes 'undo-tree-visualizer-mode)

(when (boundp 'undo-tree-visualizer-map)
  (define-key undo-tree-visualizer-map [remap evil-backward-char] 'undo-tree-visualize-switch-branch-left)
  (define-key undo-tree-visualizer-map [remap evil-forward-char] 'undo-tree-visualize-switch-branch-right)
  (define-key undo-tree-visualizer-map [remap evil-next-line] 'undo-tree-visualize-redo)
  (define-key undo-tree-visualizer-map [remap evil-previous-line] 'undo-tree-visualize-undo))

(provide 'evil-integration)

;;; evil-integration.el ends here
