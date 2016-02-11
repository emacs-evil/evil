;;; evil-jumps.el --- Jump list implementation

;; Author: Bailey Ling <bling at live.ca>

;; Version: 1.2.10

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

;;; Code:

(require 'cl-lib)

(defgroup evil-jumps nil
  "evil-jumps configuration options."
  :prefix "evil-jumps"
  :group 'evil)

(defcustom evil-jumps-max-length 100
  "The maximum number of jumps to keep track of."
  :type 'integer
  :group 'evil-jumps)

(defcustom evil-jumps-pre-jump-hook nil
  "Hooks to run just before jumping to a location in the jump list."
  :type 'hook
  :group 'evil-jumps)

(defcustom evil-jumps-post-jump-hook nil
  "Hooks to run just after jumping to a location in the jump list."
  :type 'hook
  :group 'evil-jumps)

(defcustom evil-jumps-ignored-file-patterns '("COMMIT_EDITMSG$" "TAGS$")
  "A list of pattern regexps to match on the file path to exclude from being included in the jump list."
  :type '(repeat string)
  :group 'evil-jumps)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar evil--jumps-jumping nil)
(defvar evil--jumps-debug nil)
(defvar evil--jumps-wired nil)

(defvar evil--jumps-buffer-targets "\\*\\(new\\|scratch\\)\\*"
  "Regexp to match against `buffer-name' to determine whether it's a valid jump target.")

(defvar evil--jumps-window-jumps (make-hash-table)
  "Hashtable which stores all jumps on a per window basis.")

(defvar evil--jumps-jump-list nil
  "Printable version of `evil--jumps-window-jumps'.")

(cl-defstruct evil-jumps-struct
  jumps
  (idx -1))

(defun evil--jumps-message (format &rest args)
  (when evil--jumps-debug
    (with-current-buffer (get-buffer-create "*evil-jumps*")
      (end-of-buffer)
      (insert (apply #'format format args) "\n"))))

(defun evil--jumps-get-current (&optional window)
  (unless window
    (setq window (frame-selected-window)))
  (let* ((jump-struct (gethash window evil--jumps-window-jumps)))
    (unless jump-struct
      (setq jump-struct (make-evil-jumps-struct))
      (puthash window jump-struct evil--jumps-window-jumps))
    jump-struct))

(defun evil--jumps-get-window-jump-list ()
  (let ((struct (evil--jumps-get-current)))
    (evil-jumps-struct-jumps struct)))

(defun evil--jumps-set-window-jump-list (list)
  (let ((struct (evil--jumps-get-current)))
    (setf (evil-jumps-struct-jumps struct) list)))

(defun evil--jumps-savehist-sync ()
  "Updates the printable value of window jumps for `savehist'."
  (setq evil--jumps-jump-list
        (cl-remove-if-not #'identity
                          (mapcar #'(lambda (jump)
                                      (let* ((mark (car jump))
                                             (pos (if (markerp mark)
                                                      (marker-position mark)
                                                    mark))
                                             (file-name (cadr jump)))
                                        (if (and (not (file-remote-p file-name))
                                                 (file-exists-p file-name)
                                                 pos)
                                            (list pos file-name)
                                          nil)))
                                  (evil--jumps-get-window-jump-list)))))

(defun evil--jumps-jump-to-index (idx)
  (let ((target-list (evil--jumps-get-window-jump-list)))
    (evil--jumps-message "jumping to %s" idx)
    (evil--jumps-message "target list = %s" target-list)
    (when (and (< idx (length target-list))
               (>= idx 0))
      (run-hooks 'evil-jumps-pre-jump-hook)
      (setf (evil-jumps-struct-idx (evil--jumps-get-current)) idx)
      (let* ((place (nth idx target-list))
             (pos (car place))
             (file-name (cadr place)))
        (setq evil--jumps-jumping t)
        (if (string-match-p evil--jumps-buffer-targets file-name)
            (switch-to-buffer file-name)
          (find-file file-name))
        (setq evil--jumps-jumping nil)
        (goto-char pos)
        (run-hooks 'evil-jumps-post-jump-hook)))))

(defun evil--jumps-push ()
  "Pushes the current cursor/file position to the jump list."
  (let ((target-list (evil--jumps-get-window-jump-list)))
    (while (> (length target-list) evil-jumps-max-length)
      (nbutlast target-list 1))
    (let ((file-name (buffer-file-name))
          (buffer-name (buffer-name))
          (current-pos (point-marker))
          (first-pos nil)
          (first-file-name nil)
          (excluded nil))
      (when (and (not file-name)
                 (string-match-p evil--jumps-buffer-targets buffer-name))
        (setq file-name buffer-name))
      (when file-name
        (dolist (pattern evil-jumps-ignored-file-patterns)
          (when (string-match-p pattern file-name)
            (setq excluded t)))
        (unless excluded
          (when target-list
            (setq first-pos (caar target-list))
            (setq first-file-name (car (cdar target-list))))
          (unless (and (equal first-pos current-pos)
                       (equal first-file-name file-name))
            (evil--jumps-message "pushing %s on %s" current-pos file-name)
            (push `(,current-pos ,file-name) target-list)))))
    (evil--jumps-message "%s %s" (selected-window) (car target-list))
    (evil--jumps-set-window-jump-list target-list)))

(defun evil-set-jump (&optional pos)
  "Set jump point at POS.
POS defaults to point."
  (unless (or (region-active-p) (evil-visual-state-p))
    (evil-save-echo-area
      (mapc #'(lambda (marker)
                (set-marker marker nil))
            evil-jump-list)
      (setq evil-jump-list nil)
      (push-mark pos t)))

  (unless evil--jumps-jumping
    ;; clear out intermediary jumps when a new one is set
    (let* ((struct (evil--jumps-get-current))
           (target-list (evil-jumps-struct-jumps struct))
           (idx (evil-jumps-struct-idx struct)))
      (nbutlast target-list idx)
      (setf (evil-jumps-struct-jumps struct) target-list)
      (setf (evil-jumps-struct-idx struct) -1))
    (evil--jumps-push)))

(evil-define-motion evil-jump-backward (count)
  "Go to older position in jump list.
To go the other way, press \
\\<evil-motion-state-map>\\[evil-jump-forward]."
  (let ((count (or count 1)))
    (evil-motion-loop (nil count)
      (let* ((struct (evil--jumps-get-current))
             (idx (evil-jumps-struct-idx struct)))
        (evil--jumps-message "jumping back %s" idx)
        (when (= idx -1)
          (setq idx (+ idx 1))
          (setf (evil-jumps-struct-idx struct) 0)
          (evil--jumps-push))
        (evil--jumps-jump-to-index (+ idx 1))))))

(evil-define-motion evil-jump-forward (count)
  "Go to newer position in jump list.
To go the other way, press \
\\<evil-motion-state-map>\\[evil-jump-backward]."
  (let ((count (or count 1)))
    (evil-motion-loop (nil count)
      (let* ((struct (evil--jumps-get-current))
             (idx (evil-jumps-struct-idx struct)))
        (evil--jumps-jump-to-index (- idx 1))))))

(defun evil--jumps-window-configuration-hook (&rest args)
  (let* ((window-list (window-list-1 nil nil t))
         (existing-window (selected-window))
         (new-window (previous-window)))
    (when (and (not (eq existing-window new-window))
               (> (length window-list) 1))
      (let* ((target-jump-struct (evil--jumps-get-current new-window))
             (target-jump-count (length (evil-jumps-struct-jumps target-jump-struct))))
        (if (evil-jumps-struct-jumps target-jump-struct)
            (evil--jumps-message "target window %s already has %s jumps" new-window target-jump-count)
          (evil--jumps-message "new target window detected; copying %s to %s" existing-window new-window)
          (let* ((source-jump-struct (evil--jumps-get-current existing-window))
                 (source-list (evil-jumps-struct-jumps source-jump-struct)))
            (when (= (length (evil-jumps-struct-jumps target-jump-struct)) 0)
              (setf (evil-jumps-struct-idx target-jump-struct) (evil-jumps-struct-idx source-jump-struct))
              (setf (evil-jumps-struct-jumps target-jump-struct) (copy-sequence source-list)))))))
    ;; delete obsolete windows
    (maphash (lambda (key val)
               (unless (member key window-list)
                 (evil--jumps-message "removing %s" key)
                 (remhash key evil--jumps-window-jumps)))
             evil--jumps-window-jumps)))

(defun turn-on-evil-jumps-mode ()
  (unless evil--jumps-wired
    (evil--jumps-set-window-jump-list evil--jumps-jump-list)
    (eval-after-load 'savehist
      '(progn
         (push 'evil--jumps-jump-list savehist-additional-variables)
         (add-hook 'savehist-save-hook #'evil--jumps-savehist-sync)))
    (setq evil--jumps-wired t))

  (add-hook 'next-error-hook #'evil-set-jump)
  (add-hook 'window-configuration-change-hook #'evil--jumps-window-configuration-hook)
  (defadvice switch-to-buffer (before evil-jumps activate)
    (evil-set-jump))
  (defadvice split-window-internal (before evil-jumps activate)
    (evil-set-jump))
  (defadvice find-tag-noselect (before evil-jumps activate)
    (evil-set-jump)))

(defun turn-off-evil-jumps-mode ()
  (remove-hook 'next-error-hook #'evil-set-jump)
  (remove-hook 'window-configuration-change-hook #'evil--jumps-window-configuration-hook)
  (ad-remove-advice 'switch-to-buffer 'before 'evil-jumps)
  (ad-remove-advice 'find-tag-noselect 'before 'evil-jumps))

(add-hook 'evil-local-mode-hook (lambda ()
                                  (if evil-local-mode
                                      (turn-on-evil-jumps-mode)
                                    (turn-off-evil-jumps-mode))))

(provide 'evil-jumps)

;;; evil-jumps.el ends here
