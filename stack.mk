#!/usr/bin/env make

.PHONY: build check tags style lint test exec bench doc install setup ghci clean cleanall

TARGET	:= gcd
SRCS	:= $(wildcard *.hs */*.hs)
ARGS	?= -h

.default: build

build:	check
	@stack build

all:	check build test bench doc exec

check:	tags style lint

tags:
	@hasktags --ctags --extendedctag $(SRCS)

style:
	@stylish-haskell -c .stylish-haskell.yaml -i $(SRCS)

lint:
	@hlint $(SRCS)

test:
	@stack test --coverage

exec:	# Example:  make ARGS="112 12" exec
	@stack exec $(TARGET) -- $(ARGS)

bench:
	@stack bench --benchmark-arguments '-o .stack-work/benchmark.html'

doc:
	@stack haddock

install:
	@stack install --local-bin-path $(HOME)/bin

setup:
	-stack setup
	-stack build --dependencies-only --test --no-run-tests
	-stack query
	-stack ls dependencies

ghci:
	@stack ghci --ghci-options -Wno-type-defaults

clean:
	@stack clean

cleanall: clean
	@$(RM) -rf .stack-work/
	@$(RM) -rf $(patsubst %.hs, %.hi, $(SRCS))
	@$(RM) -rf $(patsubst %.hs, %.o, $(SRCS))
