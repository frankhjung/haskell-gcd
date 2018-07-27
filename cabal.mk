#!/usr/bin/env make

TARGET = gcd
SRCS = $(wildcard *.hs */*.hs)
ARGS ?= -h

build:	check
	@cabal build

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
	@cabal check

.PHONY: exec
exec:	# Example:  make ARGS="112 12" exec
	@cabal exec $(TARGET) -- $(ARGS)

.PHONY: run
run:	# Example:  make ARGS="112 12" exec
	@cabal run $(TARGET) -- $(ARGS)

.PHONY: test
test:
	@cabal test

.PHONY: bench
bench:
	@cabal bench

.PHONY: tags
tags:
	-hasktags --ctags $(SRCS)

.PHONY: doc
doc:
	@cabal doctest
	@cabal haddock

.PHONY: copy
copy:
	@cabal copy

.PHONY: ghci
ghci:
	@ghci -Wno-type-defaults

.PHONY: clean
clean:
	@cabal clean

.PHONY: cleanall
cleanall: clean
	@$(RM) -rf */*.hi */*.o
