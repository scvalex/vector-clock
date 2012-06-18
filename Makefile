.PHONY: all build dist install test clean doc

all: build

build: dist/setup-config
	cabal build

dist: test
	cabal sdist

install: build
	cabal install

test: build
	cabal test

clean:
	cabal clean

dist/setup-config: vector-clock.cabal
	cabal configure --enable-tests

doc: build
	cabal haddock
