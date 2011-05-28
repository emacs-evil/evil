SHELL = /bin/sh
EMACS = emacs
TAG =

.PHONY: all compile clean tests test emacs term terminal indent

# Byte-compile Evil.
all: compile
compile: clean
	$(EMACS) --batch -Q -L . -f batch-byte-compile evil*.el

# Delete byte-compiled files.
clean:
	rm -f *~
	rm -f \#*\#
	rm -f *.elc

# Run all tests.
# The TAG variable may specify a test tag or a test name, e.g.:
#       make test TAG=repeat
# This will only run tests pertaining to the repeat system.
tests: clean
	$(EMACS) --batch -Q -L . -l evil-tests.el \
--eval "(evil-tests-run '(${TAG}))"

# Byte-compile Evil and run all tests.
test: compile tests

# Load Evil in a fresh instance of Emacs and run tests.
emacs: clean
	$(EMACS) -Q -L . -l evil-tests.el --eval "(evil-mode 1)" \
--eval "(if (y-or-n-p-with-timeout \"Run tests? \" 2 t) \
(evil-tests-run t) \
(message \"You can run the tests at any time with \`M-x evil-tests-run\'\"))" &

# Load Evil in the terminal and run tests.
term: terminal
terminal: clean
	$(EMACS) -nw -Q -L . -l evil-tests.el --eval "(evil-mode 1)" \
--eval "(if (y-or-n-p-with-timeout \"Run tests? \" 2 t) \
(evil-tests-run t) \
(message \"You can run the tests at any time with \`M-x evil-tests-run\'\"))"

# Re-indent all Evil code.
# Loads Evil into memory in order to indent macros properly.
# Also removes trailing whitespace, tabs and extraneous blank lines.
indent: clean
	$(EMACS) --batch evil*.el -Q -L . -l evil-tests.el \
--eval "(dolist (buffer (reverse (buffer-list))) \
(when (buffer-file-name buffer) \
(set-buffer buffer) \
(message \"Indenting %s\" (current-buffer)) \
(setq-default indent-tabs-mode nil) \
(untabify (point-min) (point-max)) \
(indent-region (point-min) (point-max)) \
(delete-trailing-whitespace) \
(untabify (point-min) (point-max)) \
(goto-char (point-min)) \
(while (re-search-forward \"\\n\\\\{3,\\\\}\" nil t) \
(replace-match \"\\n\\n\")) \
(when (buffer-modified-p) (save-buffer 0))))"
