patch: $(wildcard ../*.[ch]) w3m.el
	cd .. && cvs diff -N -r original | perl lisp/makepatch.pl >lisp/patch

clean:
	rm -f *~ *.elc patch
