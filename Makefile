PACKAGE   = w3m_el
TARBALL   = $(PACKAGE)-$(VERSION).tar.gz
DISTDIR   = $(PACKAGE)-$(VERSION)
DISTFILES = COPYING ChangeLog $(wildcard *.el)
BASEVER   = 0.2
REVISION  =
ifeq "$(REVISION)" ""
VERSION = $(BASEVER)
else
VERSION = $(BASEVER).$(REVISION)
endif

EMACS   = emacs
ECC     = $(EMACS) -q -no-site-file -batch -f batch-byte-compile

.SUFFIXES:
.SUFFIXES: .elc .el

default: $(patsubst %.el,%.elc,$(wildcard *.el))

%.elc: %.el
	$(ECC) $<

tarball:
	$(MAKE) REVISION=$(shell cvs status w3m.el|perl -ne '/Working revision:[ \t]*(?:\d+\.)+(\d+)/ and print $$1-28') dist

dist: $(TARBALL)

$(TARBALL): $(DISTFILES)
	-rm -f $(TARBALL) $(basename $(TARBALL))
	mkdir $(DISTDIR)
	cp -p $(DISTFILES) $(DISTDIR)
	tar -cf $(basename $(TARBALL)) $(DISTDIR)
	gzip -9 $(basename $(TARBALL))
	rm -rf $(DISTDIR)

patch: $(wildcard ../*.[ch]) w3m.el makepatch.pl
	cd .. && cvs diff -kk -N -r original | perl lisp/makepatch.pl >lisp/patch

clean:
	-rm -rf *~ *.elc patch w3m_el*
