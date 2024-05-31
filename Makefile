SHELL = /bin/sh
EMACS ?= emacs
SED ?= sed
FILES = $(filter-out evil-test-helpers.el evil-tests.el evil-pkg.el,$(wildcard evil*.el))
VERSION := $(shell $(SED) -ne '/define-package/,$$p' evil-pkg.el | $(SED) -ne '/^\s*"[[:digit:]]\+\(\.[[:digit:]]\+\)*"\s*$$/ s/^.*"\(.*\)".*$$/\1/p')
PROFILER =
DOC = doc
TAG =
EASK ?= eask

ELCFILES = $(FILES:.el=.elc)

.PHONY: all compile docstrings doc clean tests test emacs term terminal profiler indent elpa version

# Byte-compile Evil.
all: compile

compile:
	$(EASK) compile

# Documentation.
docstrings:
	@$(EMACS) --script scripts/evil-extract-docstrings

doc: docstrings
	@$(MAKE) -C doc texinfo

info: doc
	@$(MAKE) -C doc info

# Delete byte-compiled files etc.
clean:
	$(EASK) clean all

# Run tests.
# The TAG variable may specify a test tag or a test name:
#       make test TAG=repeat
# This will only run tests pertaining to the repeat system.
test:
	$(EASK) run command test

# Byte-compile Evil and run all tests.
tests: compile
	$(EASK) run command test
	$(EASK) clean elc
	rm -f .depend

# Load Evil in a fresh instance of Emacs and run all tests.
emacs:
	$(EMACS) -Q -L . -l evil-tests.el \
		--eval "(evil-mode 1)" \
		--eval "(evil-tests-initialize '(${TAG}) '(${PROFILER}) t)"

# Load Evil in a terminal Emacs and run all tests.
term: terminal
terminal:
	$(EASK) run command terminal

# Run all tests with profiler.
profiler:
	$(EASK) run command profiler

# Re-indent all Evil code.
# Loads Evil into memory in order to indent macros properly.
# Also removes trailing whitespace, tabs and extraneous blank lines.
indent: clean
	$(EASK) run command indent

# Create an ELPA package.
elpa:
	$(EASK) package

# Change the version using make VERSION=x.y.z
version:
	@$(EMACS) --script scripts/evilupdate "${VERSION}"

# Change the version using make VERSION=x.y.z, but do not post to the newsgroup
nversion:
	@$(EMACS) --script scripts/evilupdate nonews "${VERSION}"
