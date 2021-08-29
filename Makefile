SHELL = /bin/sh
EMACS ?= emacs
PROFILER =

.PHONY: test

# Delete byte-compiled files etc.
clean:
	@rm -f *~
	@rm -f \#*\#
	@rm -f *.elc

# Run tests.
test:
	$(EMACS) -batch -Q -L . -l diff-lisp.el -l diff-lisp-myers.el -l tests/diff-lisp-tests.el
