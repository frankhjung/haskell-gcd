#!/usr/bin/env make

.PHONY: build format check tags lint test exec bench doc install setup ghci clean cleanall

TARGET	:= gcd
SRCS	:= $(wildcard *.hs */*.hs)
ARGS	?= 112 12

.PHONY:	default
default: format check build test

all:	format check build test bench doc exec

format:
	@echo format ...
	@stylish-haskell --verbose --config=.stylish-haskell.yaml --inplace $(SRCS)
	@cabal-fmt --inplace gcd.cabal

check:	tags lint

tags:
	@hasktags --ctags --extendedctag $(SRCS)

lint:
	@echo lint ...
	@hlint --color $(SRCS)
	@cabal check --verbose=3

build:
	@echo build ...
	@stack build --verbosity info --pedantic --no-test

test:
	@echo test ...
	@stack test

exec:	# Example:  make ARGS="112 12" exec
	@stack exec $(TARGET) -- $(ARGS)

bench:
	@stack bench --benchmark-arguments '-o .stack-work/benchmark.html'

doc:
	@stack haddock

setup:
	stack update
	stack path
	stack query
	stack ls dependencies

ghci:
	@stack ghci --ghci-options -Wno-type-defaults

clean:
	@stack clean
	@cabal clean

cleanall: clean
	@stack purge
	@rm -f tags
