all:
	gprbuild -p -f -Pprada/prada
	gprbuild -p -f -Pharness/harness

test:
	gprbuild -p -f -Pharness/harness

coverage:
	gprbuild -p -f -Pharness/harness -XCOVERAGE=yes
	./test_prada
	cd harness/obj; gcov ../../prada/obj/*.gcda

clean:
	gprclean -Pharness/harness
	gprclean -Pprada/prada
	-rm -rf harness/obj
	-rm -rf prada/obj
	-rm -rf prada/lib
