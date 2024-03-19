.PHONY: doc clean uninstall

.RECIPEPREFIX = >

INSTALLDIR = $(HOME)/.local/bin
PROGPATH   = $(INSTALLDIR)/strelitzia
PDF_DOC    = $(INSTALLDIR)/strelitzia-doc.pdf

$(PROGPATH): src/*.hs *.cabal
> @cabal build -O2 -p
> @cabal install --overwrite-policy=always --installdir=$(INSTALLDIR)

doc: $(PDF_DOC)

$(PDF_DOC): README.md
> @pandoc -o $(PDF_DOC) README.md

clean:
> @cabal clean

uninstall:
> @rm -fv $(PROGPATH)
> @rm -fv $(PDF_DOC)

