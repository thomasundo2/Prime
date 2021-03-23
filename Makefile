.PHONY : test
test : all hello_world.sh
	./hello_world.sh

.PHONY : all
all : clean prime.native

# We will now make the compiler
prime.native : 
	opam config exec -- \
	ocamlbuild -use-ocamlfind prime.native


##############################
#
# Prime Makefile 1.0
#

# "ocamlbuild prime.native" will also build

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
	opam config exec -- \
	ocamlbuild -clean
