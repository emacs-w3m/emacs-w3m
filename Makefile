patch: $(wildcard ../*.[ch]) w3m.el makepatch.pl
	cd .. && cvs diff -kk -N -r kokb-original | perl lisp/makepatch.pl >lisp/patch

clean:
	rm -f *~ *.elc patch
