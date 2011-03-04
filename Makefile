test:
	emacs --batch -Q -L . -l evil-tests.el -f ert-run-tests-batch-and-exit

clean:
	rm -f *~
	rm -f \#*\#
	rm -f *.elc
