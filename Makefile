all : calc.out


##############################
#
# Problem 3
#

# "ocamlbuild calc.native" will also build the calculator

calc : parser.cmo scanner.cmo calc.cmo
	ocamlc -o calc $^

%.cmo : %.ml
	ocamlc -c $<

%.cmi : %.mli
	ocamlc -c $<

scanner.ml : scanner.mll
	ocamllex $^

parser.ml parser.mli : parser.mly
	ocamlyacc $^

# Depedencies from ocamldep
calc.cmo : scanner.cmo parser.cmi ast.cmi
calc.cmx : scanner.cmx parser.cmx ast.cmi
parser.cmo : ast.cmi parser.cmi
parser.cmx : ast.cmi parser.cmi
scanner.cmo : parser.cmi
scanner.cmx : parser.cmx


##############################

TARFILES = README Makefile zip3.ml zip3.tb wordcount.mll \
	scanner.mll ast.mli parser.mly calc.ml calc.tb

hw1.tar.gz : $(TARFILES)
	cd .. && tar zcf hw1/hw1.tar.gz $(TARFILES:%=hw1/%)

.PHONY : clean
clean :
	rm -rf zip3.out wordcount.ml wordcount wordcount.out \
	*.cmi *.cmo parser.ml parser.mli scanner.ml calc.out calc
