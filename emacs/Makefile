# -*- Makefile -*-

################################################################
## Use Emacs.app if available and anothr EMACS is not specified by user

COCOA_EMACS := /Applications/Emacs.app/Contents/MacOS/Emacs

ifneq ("$(wildcard $(COCOA_EMACS))", "")
	EMACS ?= $(COCOA_EMACS)
else
	EMACS ?= emacs
endif

################################################################
## Use cask if available and another CASK is not specified by user

CASK_VERSION := $(shell EMACS="$(EMACS)" cask --version 2>/dev/null)

ifdef CASK_VERSION
	CASK ?= cask
endif

ifdef CASK
	CASK_EXEC    ?= exec
	CASK_INSTALL ?= install
endif

################################################################
## cask, emacs and flags

COMPILER := $(CASK) $(CASK_EXEC) $(EMACS)
#FLAGS    := -Q -batch -L . -L .cask/24.3.1/elpa/org-plus-contrib-20140922 -L .cask/24.3.1/elpa/calfw-20140407.2212 -l mhc
FLAGS    := -Q -batch -L . -l mhc
ELFILES  := $(wildcard *.el)
ELCFILES := $(ELFILES:el=elc)

################################################################
## Suffix rules

.SUFFIXES: .elc .el

.el.elc:
	-rm -f $@
	$(COMPILER) $(FLAGS) -f batch-byte-compile $<

################################################################
### Targets

all: setup build

build: $(ELCFILES)
#	$(COMPILER) $(FLAGS) -f batch-byte-compile *.el

clean:
	-rm -f *.elc auto-autoloads.el custom-load.el *~

setup:
	$(CASK) $(CASK_INSTALL)
