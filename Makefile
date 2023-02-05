#!/usr/bin/env make

.PHONY: build check tags style lint test exec bench doc install setup ghci clean cleanall

TARGET	:= gcd
SRCS	:= $(wildcard *.hs */*.hs)
ARGS	?= -h

.PHONY:	default
default: check build test

all:	check build test bench doc exec

check:	tags style lint

tags:
	@hasktags --ctags --extendedctag $(SRCS)

style:
	@echo style ...
	@stylish-haskell --verbose --config=.stylish-haskell.yaml --inplace $(SRCS)

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
	@stack haddock --no-haddock-deps

install:
	@stack install --local-bin-path $(HOME)/bin

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
	@rm -f tags
	@rm -f $(wildcard *.hi **/*.hi)
	@rm -f $(wildcard *.o **/*.o)
	@rm -f $(wildcard *.prof **/*.prof)
	@rm -f $(wildcard *.tix **/*.tix)

cleanall: clean
	@stack clean --full
	@$(RM) -rf .stack-work/ $(TARGET)
