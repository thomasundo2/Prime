.PHONY : test
test : all test_all.sh
	./test_all.sh

.PHONY : all
all : clean gmp prime.native gmpfunc.o structs.o

# this will serve to install the GNU multiple precision library onto our system
.PHONY : gmp
gmp:
	apt install -y libgmp-dev

# We will now make the compiler
prime.native : codegen.ml sast.ml ast.ml semant.ml scanner.mll parser.mly
	opam config exec -- \
	ocamlbuild -use-ocamlfind prime.native

# Test the GMP calls we build
gmpfunc: gmp gmpfunc.c
	cc -o gmpfunc -DBUILD_TEST gmpfunc.c -lgmp

gmpfunc.o: gmp gmpfunc.c
	cc -c gmpfunc.c

structs: structs.c
	cc -o structs -DBUILD_TEST structs.c -lgmp

structs.o: structs.c
	cc -c structs.c


# Some old stuff:
prime : parser.cmo scanner.cmo prime.cmo
	ocamlc -o prime $^

%.cmo : %.ml
	ocamlc -c $<

%.cmi : %.mli
	ocamlc -c $<

scanner.ml : scanner.mll
	ocamllex $^

parser.ml parser.mli : parser.mly
	ocamlyacc $^

# run the tests (without outputting to file)
prime.out: prime prime.tb
	./prime < prime.tb

# Depedencies from ocamldep
prime.cmo : scanner.cmo parser.cmi ast.cmi
prime.cmx : scanner.cmx parser.cmx ast.cmi
parser.cmo : ast.cmi parser.cmi
parser.cmx : ast.cmi parser.cmi
scanner.cmo : parser.cmi
scanner.cmx : parser.cmx


##############################

# TARFILES = README Makefile scanner.mll ast.mli parser.mly prime.ml prime.tb

# hw1.tar.gz : $(TARFILES)
# 	cd .. && tar zcf hw1/hw1.tar.gz $(TARFILES:%=hw1/%)

.PHONY : clean
clean :
	rm -rf *.cmi *.cmo parser.ml parser.mli scanner.ml prime.out prime
	rm -rf *.exe *.ll *.s *.test *.diff a.out gmpfunc gmpfunc.o structs structs.o
	opam config exec -- \
	ocamlbuild -clean
