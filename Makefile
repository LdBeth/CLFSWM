DESTDIR=/usr/local/

CLISP=$(shell which clisp)
SBCL=$(shell which sbcl)
CMUCL=$(shell which cmucl || which lisp)
CCL=$(shell which ccl)
ECL=$(shell which ecl)
LOADTMP=load.tmp.lisp

SRC=$(wildcard *.lisp src/*.lisp)

all: clfswm

clfswm: $(SRC)
	@echo "Please, tweak the file load.lisp to fit your needs."
	@if test -f "$(CLISP)"; then echo "Building with CLISP"; $(CLISP) -E iso-8859-1 $(LOADTMP); \
	elif test -f "$(SBCL)"; then echo "Building with SBCL"; $(SBCL) --load $(LOADTMP); \
	elif test -f "$(CMUCL)"; then echo "Building with CMUCL"; $(CMUCL) -load $(LOADTMP); \
	elif test -f "$(CCL)"; then echo "Building with CCL"; $(CCL) --load $(LOADTMP); \
	elif test -f "$(ECL)"; then echo "Building with ECL"; $(ECL) -load $(LOADTMP); \
	else echo "No Lisp found. Please, install one of CLISP, SBCL, CMUCL, CCL or ECL"; \
	fi



install: clfswm
	@echo "Installing CLFSWM in $(DESTDIR)"
	mkdir -p $(DESTDIR)
	mkdir -p $(DESTDIR)/bin
	mkdir -p $(DESTDIR)/lib/clfswm
	mkdir -p $(DESTDIR)/share/doc/clfswm
	mkdir -p $(DESTDIR)/man/man.1
	cp clfswm $(DESTDIR)/bin/
	cp -R contrib/* $(DESTDIR)/lib/clfswm/
	cp doc/* $(DESTDIR)/share/doc/clfswm/
	cp README COPYING AUTHORS $(DESTDIR)/share/doc/clfswm/
	cp clfswm.1 $(DESTDIR)/man/man.1/


uninstall:
	@echo "Uninstalling CLFSWM from $(DESTDIR)"
	rm -f $(DESTDIR)/bin/clfswm
	rm -rf $(DESTDIR)/lib/clfswm/*
	rm -f $(DESTDIR)/share/doc/clfswm/*
	rm -f $(DESTDIR)/man/man.1/clfswm.1
	rmdir $(DESTDIR)/lib/clfswm
	rmdir $(DESTDIR)/share/doc/clfswm


clean:
	rm -f clfswm

