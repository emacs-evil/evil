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
(require 'evil-search)
(require 'evil-ex)

(define-derived-mode evil-command-window-mode text-mode "Evil-cmd"
  "Major mode for the Evil command line window."
  (setq-local after-change-functions (cons 'evil-command-window-draw-prefix
                                           after-change-functions)))

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
  (ignore-errors (kill-buffer "*Command Line*"))
  (switch-to-buffer "*Command Line*")
  (evil-command-window-mode)
  (setq-local evil-command-window-execute-fn execute-fn)
  (setq-local evil-command-window-cmd-key cmd-key)
  (evil-command-window-insert-commands hist))

(defun evil-command-window-ex (&optional current-command)
  "Open a command line window for editing and executing ex commands.
If CURRENT-COMMAND is present, it will be inserted under the
cursor as the current command to be edited."
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
      (evil-ex-execute result))
    (unless (equal result (car evil-ex-history))
      (setq evil-ex-history (cons result evil-ex-history)))))

(defun evil-command-window-search-forward ()
  "Open a command line window for forward searches."
  (interactive)
  (evil-command-window (cons " " evil-search-forward-history)
                       "/"
                       (lambda (result)
                         (evil-command-window-search-execute result t))))

(defun evil-command-window-search-backward ()
  "Open a command line window for backward searches."
  (interactive)
  (evil-command-window (cons " " evil-search-backward-history)
                       "?"
                       (lambda (result)
                         (evil-command-window-search-execute result nil))))

(defun evil-command-window-search-execute (result forward)
  "Search for RESULT using FORWARD to determine direction."
  (unless (equal result " ")
    (if (and (boundp 'evil-search-module) (eq evil-search-module 'evil-search))
        (progn
          (setq evil-ex-search-pattern (evil-ex-make-search-pattern result)
                evil-ex-search-direction (if forward 'forward 'backward))
          (evil-ex-search))
      (evil-search result forward evil-regexp-search))))

(defun evil-command-window-draw-prefix (&rest ignored)
  "Display `evil-command-window-cmd-key' as a prefix to the current line.
Parameters passed in through IGNORED are ignored."
  (let ((prefix (propertize evil-command-window-cmd-key
                            'font-lock-face 'minibuffer-prompt)))
    (set-text-properties (line-beginning-position) (line-end-position)
                         (list 'line-prefix prefix))))

(defun evil-command-window-insert-commands (hist)
  "Insert the commands in HIST."
  (let ((inhibit-modification-hooks t))
    (mapc #'(lambda (cmd) (insert cmd) (newline)) hist)
    (join-line)
    (delete-char 1))
  (reverse-region (point-min) (point-max)) ; draws prefixes as a side-effect
  (goto-char (point-max))
  (evil-adjust-cursor))

(provide 'evil-command-window)

;;; evil-command-window.el ends here
