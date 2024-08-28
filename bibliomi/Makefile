.PHONY: clean uninstall

.RECIPEPREFIX = >

INSTALLDIR = $(HOME)/.local/bin
PROG_PATH = $(INSTALLDIR)/bibliomi

$(PROG_PATH): $(wildcard src/*.hs) *.cabal
> @cabal build
> @cabal install --overwrite-policy=always --installdir=$(INSTALLDIR)

clean:
> @cabal clean

uninstall:
> @rm -fv $(PROG_PATH)

