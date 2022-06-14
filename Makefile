export EMACS ?= $(shell which emacs)

.PHONY: install
install:
	eask package
	eask install

.PHONY: compile
compile:
	eask compile

.PHONY: test
test:
	eask install-deps --dev
	eask test ert ./fzf-native-test.el

.PHONY: lint
lint:
	eask lint package
