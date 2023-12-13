.PHONY: clean uninstall

.RECIPEPREFIX = >

INSTALLDIR := $(HOME)/.local/bin
PROGPATH := $(INSTALLDIR)/flytex

$(PROGPATH): src/*.hs *.cabal
> @cabal build -O2 -p
> @cabal install --overwrite-policy=always --installdir=$(INSTALLDIR)

clean:
> @cabal clean

uninstall:
> @rm $(PROGPATH)

