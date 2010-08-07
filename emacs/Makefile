# -*- Makefile -*-

# MEW_OPTS  = --with-mew
# WL_OPTS   = --with-wl
# GNUS_OPTS = --with-gnus

## To change install directory.
# INST_OPTS = --with-lispdir=/usr/local/share/emacs/site-lisp/mhc

## To add paths of additional emacs-lisp libraries.
# INST_OPTS = --with-addpath=/foo:/bar:/baz

EMACS  = emacs
XEMACS = xemacs
FLAGS  = -batch -q -no-site-file -l MHC-MK
OPTS   = $(INST_OPTS) $(MEW_OPTS) $(WL_OPTS) $(GNUS_OPTS)

elc:
	$(EMACS) $(FLAGS) -f make-mhc-compile $(OPTS)

install:
	$(EMACS) $(FLAGS) -f make-mhc-install $(OPTS) # $(MAKE)

package:
	$(XEMACS) $(FLAGS) -f make-mhc-compile-package $(OPTS)

install-package:
	$(XEMACS) $(FLAGS) -f make-mhc-install-package $(OPTS) # $(MAKE)

clean:
	-rm -f *.elc auto-autoloads.el custom-load.el

### Suffix rules
.SUFFIXES: .elc .el

.el.elc:
	-rm -f $@
	$(EMACS) -batch -q -no-site-file \
		-eval '(setq load-path (cons default-directory load-path))' \
		-l mhc.el -f batch-byte-compile $<
