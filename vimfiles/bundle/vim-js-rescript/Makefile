.PHONY: test test-ci

MYVIM ?= nvim --headless

INMAKE := 1
export INMAKE

test:
	@$(MYVIM) -u ./test/test_all.vim
