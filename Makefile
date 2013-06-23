CABAL := $(shell cabal-dev --version > /dev/null && echo cabal-dev || echo cabal)

all: build test

.PHONY: all build dist install clean doc site p

build: dist/setup-config
	rm -rf _site _cache
	$(CABAL) build

dist:
	$(CABAL) sdist

install: build
	cabal install --force-reinstalls

clean:
	$(CABAL) clean
	rm -rf cabal-dev/

dist/setup-config: vector-clock.cabal
# If you don't have all the necessary packages installed on the first
# run, run `cabal-dev install`.
	$(CABAL) configure --enable-tests || $(CABAL) install --enable-tests

doc: build
	$(CABAL) haddock

test: build
	$(CABAL) test

p:
	permamake.sh $(shell find src/ -name '*.hs') \
                     $(shell find test/ -name '*.hs') \
                     *.cabal \
                     Makefile
