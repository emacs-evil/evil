all: compile

compile: clean
	emacs --batch -Q -L . -f batch-byte-compile evil*.el

test:
	emacs --batch -Q -L . -l evil-tests.el -f ert-run-tests-batch-and-exit

clean:
	rm -f *~
	rm -f \#*\#
	rm -f *.elc
