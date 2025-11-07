EMACS ?= emacs

.PHONY: test check

test check:
	$(EMACS) --batch -Q -L . -l test/org-tag-tree-test.el -f ert-run-tests-batch-and-exit
