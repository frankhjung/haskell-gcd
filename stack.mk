#!/usr/bin/env make

TARGET	:= gcd
SRCS	:= $(wildcard *.hs */*.hs)
ARGS	?= -h

.default: build

.PHONY: build
build:	check
	@stack build

.PHONY: all
all:	check build test bench doc

.PHONY: check
check:	tags lint style

.PHONY: tags
tags:
	@hasktags --ctags $(SRCS)

.PHONY: style
style:
	@stylish-haskell -c .stylish-haskell.yaml -i $(SRCS)

.PHONY: lint
lint:
	@hlint --color $(SRCS)

.PHONY: test
test:
	@stack test --coverage

.PHONY: bench
bench:
	@stack bench --benchmark-arguments '-o .stack-work/benchmark.html'

.PHONY: exec
exec:	# Example:  make ARGS="112 12" exec
	@stack exec $(TARGET) -- $(ARGS)

.PHONY: install
install:
	@stack install --local-bin-path $(HOME)/bin

.PHONY: doc
doc:
	@stack haddock

.PHONY: setup
setup:
	-stack setup
	-stack build --dependencies-only --test --no-run-tests
	-stack query
	-stack ls dependencies

.PHONY: ghci
ghci:
	@stack ghci --ghci-options -Wno-type-defaults

.PHONY: clean
clean:
	@stack clean

.PHONY: cleanall
cleanall: clean
	@$(RM) -rf .stack-work/
	@$(RM) -rf $(patsubst %.hs, %.hi, $(SRCS))
	@$(RM) -rf $(patsubst %.hs, %.o, $(SRCS))
