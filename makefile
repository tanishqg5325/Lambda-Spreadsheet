make:
	ocamlc -c backend.ml
	ocamlyacc parser.mly
	ocamlc -c parser.mli
	ocamlc -c parser.ml
	ocamllex lexer.mll
	ocamlc -c lexer.ml
	ocamlc -o excel lexer.cmo parser.cmo backend.cmo str.cma test.ml

clean:
	rm lexer.ml parser.mli parser.ml excel
	rm *.cm*
