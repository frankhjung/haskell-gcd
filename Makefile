#!/usr/bin/env make

TARGET = gcd
SRCS = $(wildcard *.hs */*.hs)

build:	check
	@stack build

.PHONY: all
all:	check build test bench doc tags

style:	$(SRCS)
	@stylish-haskell -c .stylish-haskell.yaml -i $(SRCS)

lint:	$(SRCS)
	@hlint --color $(SRCS)

check:	lint style

tags:	build
	@hasktags --ctags $(SRCS)

exec:	# Example:  make ARGS="112 12" exec
	@stack exec -- $(TARGET) $(ARGS)

.PHONY: test
test:
	@stack test

.PHONY: bench
bench:
	@stack bench

.PHONY: install
install:
	@stack install

.PHONY: doc
doc:
	@stack haddock

.PHONY: clean
clean:
	@stack clean

.PHONY: cleanall
cleanall: clean
	@$(RM) -rf .stack-work/

.PHONY: ghci
ghci:
	@stack ghci --ghci-options -Wno-type-defaults
