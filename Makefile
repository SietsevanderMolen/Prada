.PHONY: prada pradainstall test coverage clean

all: prada pradainstall coverage

prada:
	gprbuild -p -f -P./prada/prada

pradainstall:
	gprbuild -p -f -P./prada/pradainstall

install:
	install -Dm755 exe/prada ${DESTDIR}/usr/bin/prada
	install -Dm755 exe/pradainstall ${DESTDIR}/usr/bin/pradainstall

test:
	gprbuild -p -f -P./harness/harness

coverage:
	gprbuild -p -f -P./harness/harness -XCOVERAGE=yes
	exe/test_prada
	cd harness/obj; gcov ../../prada/obj/*.gcda

clean:
	gprclean -P./harness/harness
	gprclean -P./prada/prada
	gprclean -P./prada/pradainstall
	-rm -rf ./harness/obj
	-rm -rf ./prada/obj
	-rm -rf ./prada/lib
