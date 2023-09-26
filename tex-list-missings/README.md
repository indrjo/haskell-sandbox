# tex-list-missings


## Description

Assume you have a *minimal* TeX Live and want to compile your project into, say a pdf, but your TeX Live is, well, not complete. Hit Enter until the selected TeX engine has terminated: there is log file now. It is time for `tex-list-missings`.


## Usage

The usage is nothing sophisticated, it requires one file as argument:

```sh
$ tex-list-missings FILE.tex
```

Of course, you can easily install all the missing packages:

```sh
$ tex-list-missings FILE.tex | xargs tlmgr install 
```


## Install & uninstall

There is a `Makefile` in this repo.

```sh
make             # install
make uninstall   # uninstall
```


## External tools used by the program

This program uses external tools, during its execution.

* `kpsewhich` to check if a package is already installed. It should be part of TeX Live

