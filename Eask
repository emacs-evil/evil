;; -*- mode: eask; lexical-binding: t -*-

(package "evil"
         "1.15.0"
         "Extensible vi layer")

(website-url "https://github.com/emacs-evil/evil")
(keywords "emulations")

(package-file "evil.el")
(files "evil-*.el" '(:exclude "*-tests.el" "*-pkg.el"))

(script "test" "echo \"Error: no test specified\" && exit 1")

(source 'gnu)
(source 'melpa)

(depends-on "emacs" "24.1")
(depends-on "cl-lib")
(depends-on "goto-chg")
(depends-on "nadvice")

(setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432

(eask-defcommand test
  "Basic tests."
  (require 'evil-tests)
  (evil-tests-initialize nil nil))

(eask-defcommand terminal
  "Load Evil in a terminal Emacs and run all tests."
  (require 'evil-tests)
  (evil-tests-initialize nil nil t))

(eask-defcommand profiler
  "Run all tests with profiler."
  (require 'evil-tests)
  (evil-mode 1)
  (evil-tests-initialize nil t))

(eask-defcommand indent
  "Re-indent all Evil code."
  (setq vc-handled-backends nil)
  (dolist (file (eask-package-el-files)) (load file))
  (dolist (buffer (reverse (buffer-list)))
    (when (buffer-file-name buffer)
      (set-buffer buffer)
      (message "Indenting %s" (current-buffer))
      (setq-default indent-tabs-mode nil)
      (untabify (point-min) (point-max))
      (indent-region (point-min) (point-max))
      (delete-trailing-whitespace)
      (untabify (point-min) (point-max))
      (goto-char (point-min))
      (while (re-search-forward "\\n\\\\{3,\\\\}" nil t)
        (replace-match "\\n\\n"))
      (when (buffer-modified-p) (save-buffer 0)))))
