all:
	gprbuild -p -f -P./prada/prada
	gprbuild -p -f -P./harness/harness

install:
	install -Dm755 exe/prada ${DESTDIR}/usr/bin/prada

test:
	gprbuild -p -f -P./harness/harness

coverage:
	gprbuild -p -f -P./harness/harness -XCOVERAGE=yes
	exe/test_prada
	cd harness/obj; gcov ../../prada/obj/*.gcda

clean:
	gprclean -P./harness/harness
	gprclean -P./prada/prada
	-rm -rf ./harness/obj
	-rm -rf ./prada/obj
	-rm -rf ./prada/lib
