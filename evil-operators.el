;;;; Operator-Pending state

(require 'evil-states)

(evil-define-state operator
  "Operator-Pending state"
  :tag "<O>"
  :enable (normal evil-operator-shortcut-mode))

(evil-define-mode evil-operator-shortcut-mode
  "Keymap for Operator-Pending shortcuts like \"dd\" and \"gqq\"."
  :local t)

(provide 'evil-operators)

;;; evil-operators.el ends here
