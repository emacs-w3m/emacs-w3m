PACKAGE   = w3m_el
VERSION   = 0.2
TARBALL   = $(PACKAGE)-$(VERSION).tar.gz
DISTDIR   = $(PACKAGE)-$(VERSION)
DISTFILES = COPYING ChangeLog $(wildcard *.el)

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
