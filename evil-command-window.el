;;; evil-command-window.el --- Evil command line window implementation
;; Author: Emanuel Evans <emanuel.evans at gmail.com>
;; Maintainer: Vegard Øye <vegard_oye at hotmail.com>

;; Version: 1.0.9

;;
;; This file is NOT part of GNU Emacs.

;;; License:

;; This file is part of Evil.
;;
;; Evil is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Evil is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Evil.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This provides an implementation of the vim command line window for
;; editing and repeating past ex commands and searches.

;;; Code:

(require 'evil-vars)
(require 'evil-common)
(require 'evil-ex)

(define-derived-mode evil-command-window-mode text-mode "Evil-cmd"
  "Major mode for the Evil command line window.")

(defun evil-command-window (hist cmd-key execute-fn)
  "Open a command line window for HIST with CMD-KEY and EXECUTE-FN.
HIST should be a list of commands.  CMD-KEY should be the string of
the key whose history is being shown (one of \":\", \"/\", or
\"?\").  EXECUTE-FN should be a function of one argument to
execute on the result that the user selects."
  (when (eq major-mode 'evil-command-window-mode)
    (error "Cannot recursively open command line window"))
  (mapc #'(lambda (win)
            (when (equal (buffer-name (window-buffer win))
                         "*Command Line*")
              (kill-buffer (window-buffer win))
              (delete-window win)))
        (window-list))
  (split-window nil
                (unless (zerop evil-command-window-height)
                  evil-command-window-height)
                'above)
  (setq evil-command-window-current-buffer (current-buffer))
  (switch-to-buffer "*Command Line*")
  (erase-buffer)
  (evil-command-window-mode)
  (setq-local evil-command-window-execute-fn execute-fn)
  (evil-command-window-insert-commands hist cmd-key))

(defun evil-command-window-ex (&optional current-command)
  "Open a command line window for editing and executing ex commands."
  (interactive)
  (evil-command-window (cons (or current-command " ") evil-ex-history)
                       ":"
                       'evil-command-window-ex-execute))

(defun evil-command-window-execute ()
  "Execute the command under the cursor in the appropriate buffer.
The local var `evil-command-window-execute-fn' determines which
function to execute."
  (interactive)
  (let ((result (buffer-substring (line-beginning-position)
                                  (line-end-position)))
        (execute-fn evil-command-window-execute-fn)
        (command-window (get-buffer-window)))
    (select-window (previous-window))
    (unless (equal evil-command-window-current-buffer (current-buffer))
      (error "Originating buffer is no longer active"))
    (kill-buffer "*Command Line*")
    (delete-window command-window)
    (funcall execute-fn result)
    (setq evil-command-window-current-buffer nil)))

(defun evil-command-window-ex-execute (result)
  "Execute RESULT as an ex command in the appropriate buffer."
  (unless (string-match-p "^ *$" result)
    (let ((evil-ex-current-buffer evil-command-window-current-buffer))
      (evil-ex-execute result))))

(defun evil-command-window-set-margin (cmd-key)
  "Display CMD-KEY as a prefix to all lines in the margin if possible."
  ;; TODO: Make compatible with linum-mode
  (unless (and (boundp 'linum-mode) linum-mode)
    (set-window-margins (get-buffer-window) (string-width cmd-key))
    (goto-char (point-min))
    (while (not (eobp))
      (let ((overlay (make-overlay (point) (point)))
            (prefix
             (propertize cmd-key 'font-lock-face 'minibuffer-prompt)))
        (overlay-put overlay 'before-string
                     (propertize " " 'display
                                 (list '(margin left-margin) prefix))))
      (forward-line))))

(defun evil-command-window-insert-commands (hist cmd-key)
  "Insert the commands in HIST, with CMD-KEY displayed in the margin."
  (mapc #'(lambda (cmd) (insert cmd) (newline)) hist)
  (join-line)
  (reverse-region (point-min) (point-max))
  (evil-command-window-set-margin cmd-key)
  (goto-char (point-max))
  (evil-adjust-cursor))

(provide 'evil-command-window)

;;; evil-command-window.el ends here
