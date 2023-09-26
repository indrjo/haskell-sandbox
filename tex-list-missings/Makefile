.PHONY: clean uninstall

.RECIPEPREFIX = >

INSTALLDIR = $(HOME)/.local/bin
PROGPATH   = $(INSTALLDIR)/list-missings

$(PROGPATH): src/*.hs *.cabal
> @cabal build
> @cabal install --overwrite-policy=always --installdir=$(INSTALLDIR)

clean:
> @cabal clean

uninstall:
> @rm $(PROGPATH)

