SHELL = /bin/bash
EMACS = emacs
FILES = evil*.el
TAG =

.PHONY: all compile compile-batch clean tests test emacs term terminal indent

# Byte-compile Evil.
all: compile
compile: clean
	for f in ${FILES}; do \
  $(EMACS) --batch -Q -L . -L lib -f batch-byte-compile $$f; \
done

# Byte-compile all files in one batch. This is faster than
# compiling each file in isolation, but also less stringent.
compile-batch: clean
	$(EMACS) --batch -Q -L . -L lib -f batch-byte-compile ${FILES}

# Delete byte-compiled files.
clean:
	rm -f *~
	rm -f \#*\#
	rm -f *.elc

# Run tests.
# The TAG variable may specify a test tag or a test name:
#       make test TAG=repeat
# This will only run tests pertaining to the repeat system.
test: clean
	$(EMACS) --batch -Q -L . -L lib -l evil-tests.el \
--eval "(evil-tests-run '(${TAG}))"

# Byte-compile Evil and run all tests.
tests: compile-batch
	$(EMACS) --batch -Q -L . -L lib -l evil-tests.el \
--eval "(evil-tests-run '(${TAG}))"
	rm -f *.elc

# Load Evil in a fresh instance of Emacs and run all tests.
emacs: clean
	$(EMACS) -Q -L . -L lib -l evil-tests.el --eval "(evil-mode 1)" \
--eval "(if (y-or-n-p-with-timeout \"Run tests? \" 2 t) \
(evil-tests-run '(${TAG}) t) \
(message \"You can run the tests at any time with \`M-x evil-tests-run\'\"))" &

# Load Evil in a terminal Emacs and run all tests.
term: terminal
terminal: clean
	$(EMACS) -nw -Q -L . -L lib -l evil-tests.el --eval "(evil-mode 1)" \
--eval "(if (y-or-n-p-with-timeout \"Run tests? \" 2 t) \
(evil-tests-run '(${TAG}) t) \
(message \"You can run the tests at any time with \`M-x evil-tests-run\'\"))"

# Re-indent all Evil code.
# Loads Evil into memory in order to indent macros properly.
# Also removes trailing whitespace, tabs and extraneous blank lines.
indent: clean
	$(EMACS) --batch ${FILES} -Q -L . -L lib -l evil-tests.el \
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
