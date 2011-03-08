SHELL = /bin/sh

.PHONY: all
all: compile

.PHONY: compile
compile: clean
	emacs --batch -Q -L . -f batch-byte-compile evil*.el

.PHONY: test
test: compile tests

.PHONY: tests
tests:
	emacs --batch -Q -L . -l evil-tests.el -f ert-run-tests-batch-and-exit

.PHONY: clean
clean:
	rm -f *~
	rm -f \#*\#
	rm -f *.elc

.PHONY: emacs
emacs: clean
	emacs -Q -L . -l evil-tests.el --eval "(evil-mode 1)" --eval "(ert-run-tests-interactively t)" &

.PHONY: term
term: terminal

.PHONY: terminal
terminal: clean
	emacs -nw -Q -L . -l evil-tests.el --eval "(evil-mode 1)" --eval "(ert-run-tests-interactively t)"
