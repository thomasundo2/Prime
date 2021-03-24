.PHONY: all
all: scanner.exe
	./scanner.exe scanner1.scan

scanner.exe:
	ocamllex scanner_test.mll
	ocamlc scanner_test.ml -o scanner.exe

.PHONY: clean
clean:
	rm -rf *.exe *.cmi *.cmo scanner.exe scanner_test.ml
