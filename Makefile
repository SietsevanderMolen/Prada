.PHONY: prada install test coverage clean

prada:
	gprbuild -p -f -P./prada/prada
	gprbuild -p -f -P./prada/pradainstall

install:
	install -Dm755 exe/prada ${DESTDIR}/usr/bin/prada
	install -Dm755 exe/pradainstall ${DESTDIR}/usr/bin/pradainstall

test: prada
	gprbuild -p -f -P./harness/harness

coverage: prada
	gprbuild -p -f -P./harness/harness -XCOVERAGE=yes
	exe/test_prada
	cd harness/obj; gcov ../../prada/obj/pradainstall.gcda
	cd harness/obj; gcov ../../prada/obj/prada.gcda

clean:
	gprclean -P./harness/harness
	gprclean -P./prada/prada
	gprclean -P./prada/pradainstall
