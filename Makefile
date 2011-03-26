SHELL = /bin/bash

.PHONY: all compile indent test tests clean emacs term terminal

all: compile

compile: clean
	emacs --batch -Q -L . -f batch-byte-compile evil*.el

indent:
	emacs --batch evil*.el -Q -L . -l evil-tests.el \
--eval "(dolist (buffer (reverse (buffer-list))) \
(when (buffer-file-name buffer) \
(set-buffer buffer) \
(message \"Indenting %s\" (current-buffer)) \
(setq-default indent-tabs-mode nil) \
(untabify (point-min) (point-max)) \
(indent-region (point-min) (point-max)) \
(delete-trailing-whitespace) \
(when (buffer-modified-p) (save-buffer 0))))"

test: compile tests

tests:
	emacs --batch -Q -L . -l evil-tests.el -f ert-run-tests-batch-and-exit

clean:
	rm -f *~
	rm -f \#*\#
	rm -f *.elc

emacs: clean
	emacs -Q -L . -l evil-tests.el --eval "(evil-mode 1)" \
--eval "(if (y-or-n-p-with-timeout \"Run tests? \" 2 t) \
(ert-run-tests-interactively t) \
(message \"You can run the tests at any time with \`M-x evil-tests-run\'\"))" &

term: terminal

terminal: clean
	emacs -nw -Q -L . -l evil-tests.el --eval "(evil-mode 1)" --eval "(ert-run-tests-interactively t)"
