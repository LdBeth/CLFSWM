DESTDIR=/usr/local/

LOAD=load.lisp

all: clfswm

clfswm:
	@echo "Please, tweak the file load.lisp to fit your needs."
	@clisp -E iso-8859-1 $(LOAD) || \
	sbcl --load $(LOAD) || \
	cmucl -load $(LOAD) || lisp -load $(LOAD) || \
	ccl --load $(LOAD) || \
	ecl -load $(LOAD) || \
	echo "No Lisp found. Please, install one of CLISP, SBCL, CMUCL, CCL or ECL"


install: clfswm
	@echo "Installing CLFSWM in $(DESTDIR)"
	mkdir -p $(DESTDIR)
	mkdir -p $(DESTDIR)/bin
	mkdir -p $(DESTDIR)/lib/clfswm
	mkdir -p $(DESTDIR)/share/doc/clfswm
	mkdir -p $(DESTDIR)/man/man.1
	mkdir -p $(DESTDIR)/share/applications
	mkdir -p $(DESTDIR)/share/xsessions
	cp clfswm $(DESTDIR)/bin/
	cp -R contrib/* $(DESTDIR)/lib/clfswm/
	cp doc/* $(DESTDIR)/share/doc/clfswm/
	cp README COPYING AUTHORS $(DESTDIR)/share/doc/clfswm/
	cp clfswm.1 $(DESTDIR)/man/man.1/
	cp clfswm.desktop $(DESTDIR)/share/applications/
	cp clfswm-session.desktop $(DESTDIR)/share/xsessions/


uninstall:
	@echo "Uninstalling CLFSWM from $(DESTDIR)"
	rm -f $(DESTDIR)/bin/clfswm
	rm -rf $(DESTDIR)/lib/clfswm/*
	rm -f $(DESTDIR)/share/doc/clfswm/*
	rm -f $(DESTDIR)/man/man.1/clfswm.1
	rm -f $(DESTDIR)/share/applications/clfswm.desktop
	rm -f $(DESTDIR)/share/xsessions/clfswm-session.desktop
	rmdir $(DESTDIR)/lib/clfswm
	rmdir $(DESTDIR)/share/doc/clfswm


clean:
	rm -f clfswm

