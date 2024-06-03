.PHONY: doc clean uninstall

.RECIPEPREFIX = >

INSTALLDIR = $(HOME)/.bin
PROG_PATH = $(INSTALLDIR)/change-math-delims

$(PROG_PATH): Main.hs *.cabal
> @cabal build -O2 -p
> @cabal install --overwrite-policy=always --installdir=$(INSTALLDIR)

clean:
> @cabal clean

uninstall:
> @rm -fv $(PROG_PATH)

