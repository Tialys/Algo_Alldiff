
build:
	ocamlc alldiff.ml test.ml -o test

run:
	./test
clean:
	rm -rf *.cmi *.cmo *~
