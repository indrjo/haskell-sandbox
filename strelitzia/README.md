# Strelitzia

All the code here used to be part of another repo, namely [minimal TeX Live](https://github.com/indrjo/minimal-texlive-installer.git). The program *strelitzia* has the same purpose: to install packages *on the fly*.

This program is written in [Haskell](https://www.haskell.org/), so you need all the machinery to compile the source code: if you do not have it, [click!](https://www.haskell.org/ghcup/)

The whole work in this repository is *cabal*ised, hence during the build time all the necessary modules are downloaded and used. There is a small `Makefile`, so use it.

A simple `make` on your terminal installs the program. At the end, you will have `~/.local/bin/strelitzia`, so make sure `~/.local/bin` is in your `PATH`. Indeed, `make uninstall` just gets rid of the program in that location.


## Usage

The most basic usage is the following:

```sh
$ strelitzia --i main.tex
```

which will try to compile the file `main.tex` with `pdflatex`. If you want another TeX engine, you have the flag `--c` to do so: for example

```sh
$ strelitzia --c lualatex --i main.tex
```

The switch `--o` is for the options to be passed to the selected TeX engine: for example

```sh
$ strelitzia --c lualatex --o "--synctex=1 --shell-escape" --i main.tex
```

In general, always refer to `strelitzia --help`, as it may happen the latest changes are not documented yet.


## Troubleshooting

In general, refer to the log of the program. Every error will be captured and reported to you.

...
