GHCFLAGS=-Wall -fno-warn-tabs -fno-warn-name-shadowing -XHaskell2010
HLINTFLAGS=-XHaskell2010 -XCPP -i 'Use camelCase' -i 'Use String' -i 'Use string literal' -i 'Use list comprehension'
VERSION=0.4.0

.PHONY: all clean doc install test

all: report.html test doc dist/build/libHSunexceptionalio-$(VERSION).a dist/unexceptionalio-$(VERSION).tar.gz

install: dist/build/libHSunexceptionalio-$(VERSION).a
	cabal install

report.html: UnexceptionalIO.hs
	-hlint $(HLINTFLAGS) --report UnexceptionalIO.hs

doc: dist/doc/html/unexceptionalio/index.html README

test: UnexceptionalIO.hs tests/suite.hs
	runhaskell -- $(GHCFLAGS) -fno-warn-missing-methods -fno-warn-missing-fields tests/suite.hs

README: unexceptionalio.cabal
	tail -n+$$(( `grep -n ^description: $^ | head -n1 | cut -d: -f1` + 1 )) $^ > .$@
	head -n+$$(( `grep -n ^$$ .$@ | head -n1 | cut -d: -f1` - 1 )) .$@ > $@
	-printf ',s/        //g\n,s/^.$$//g\nw\nq\n' | ed $@
	$(RM) .$@

dist/doc/html/unexceptionalio/index.html: dist/setup-config UnexceptionalIO.hs 
	cabal haddock --hyperlink-source

dist/setup-config: unexceptionalio.cabal
	cabal configure

clean:
	find -name '*.o' -o -name '*.hi' | xargs $(RM)
	$(RM) report.html
	$(RM) -r dist dist-ghc

dist/build/libHSunexceptionalio-$(VERSION).a: unexceptionalio.cabal dist/setup-config UnexceptionalIO.hs
	cabal build --ghc-options="$(GHCFLAGS)"

dist/unexceptionalio-$(VERSION).tar.gz: unexceptionalio.cabal dist/setup-config UnexceptionalIO.hs README
	cabal check
	cabal sdist
