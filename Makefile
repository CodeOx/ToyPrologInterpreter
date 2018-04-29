lexer: lexer.mll parser.mly interpreter.ml
	ocamlc -c prologInterpreter.ml
	ocamllex lexer.mll
	ocamlyacc parser.mly
	ocamlc -c parser.mli
	ocamlc -c lexer.ml
	ocamlc -c parser.ml
	ocamlc -c interpreter.ml
	ocamlc -o main.exe prologInterpreter.cmo lexer.cmo parser.cmo interpreter.cmo

clean:
	rm lexer.ml lexer.cmi lexer.cmo parser.cmi parser.cmo parser.ml parser.mli interpreter.cmi interpreter.cmo main.exe
