.PHONY: libaurinterface install test coverage clean

prada: libaurinterface
	gprbuild -p -P./prada/prada
	gprbuild -p -P./prada/pradainstall
	gprbuild -p -P./prada/pradaupdate

libaurinterface:
	gprbuild -p -P./prada/libaurinterface

install:
	install -Dm755 exe/prada ${DESTDIR}/usr/bin/prada
	install -Dm755 exe/pradainstall ${DESTDIR}/usr/bin/pradainstall
	install -Dm755 exe/pradaupdate ${DESTDIR}/usr/bin/pradaupdate

test: prada
	gprbuild -p -f -P./harness/harness

coverage: prada
	gprbuild -p -f -P./harness/harness -XCOVERAGE=yes
	exe/test_prada
	cd harness/obj; gcov ../../prada/obj/pradaupdate.gcda
	cd harness/obj; gcov ../../prada/obj/pradainstall.gcda
	cd harness/obj; gcov ../../prada/obj/prada.gcda

clean:
	gprclean -P./harness/harness
	gprclean -P./prada/prada
	gprclean -P./prada/pradainstall
