#!/usr/bin/env make

TARGET = gcd
SRCS = $(wildcard *.hs */*.hs)
ARGS ?= -h

build:	check
	@stack build

.PHONY: all
all:	check build test bench doc tags

.PHONY: style
style:
	@stylish-haskell -c .stylish-haskell.yaml -i $(SRCS)

.PHONY: lint
lint:
	@hlint --color $(SRCS)

.PHONY: check
check:	lint style

.PHONY: exec
exec:	# Example:  make ARGS="112 12" exec
	@stack exec $(TARGET) -- $(ARGS)

.PHONY: test
test:
	@stack test

.PHONY: bench
bench:
	@stack bench

.PHONY: tags
tags:
	@hasktags --ctags $(SRCS)

.PHONY: install
install:
	@stack install

.PHONY: doc
doc:
	@stack haddock

.PHONY: ghci
ghci:
	@stack ghci --ghci-options -Wno-type-defaults

.PHONY: clean
clean:
	@stack clean

.PHONY: cleanall
cleanall: clean
	@$(RM) -rf .stack-work/
