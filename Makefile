

all:    runtime.s.bc
	/usr/bin/ocamldep -modules main.ml > main.ml.depends
	/usr/bin/ocamldep -modules env.ml > env.ml.depends
	/usr/bin/ocamllex -q lexer.mll
	/usr/bin/ocamldep -modules lexer.ml > lexer.ml.depends
	/usr/bin/ocamlyacc parser.mly
	/usr/bin/ocamldep -modules parser.mli > parser.mli.depends
	/usr/bin/ocamldep -modules syntax.ml > syntax.ml.depends
	/usr/bin/ocamlc -c -dtypes -I tests -o env.cmo env.ml
	/usr/bin/ocamlc -c -dtypes -I tests -o syntax.cmo syntax.ml
	/usr/bin/ocamlc -c -dtypes -I tests -o parser.cmi parser.mli
	/usr/bin/ocamldep -modules semantics.ml > semantics.ml.depends
	/usr/bin/ocamldep -modules compileHelpers.ml > compileHelpers.ml.depends
	/usr/bin/ocamlc -c -dtypes -I tests -o compileHelpers.cmo compileHelpers.ml
	/usr/bin/ocamlc -c -dtypes -I tests -o lexer.cmo lexer.ml
	/usr/bin/ocamlc -c -dtypes -I tests -o semantics.cmo semantics.ml
	/usr/bin/ocamlc -c -dtypes -I tests -o main.cmo main.ml
	/usr/bin/ocamldep -modules parser.ml > parser.ml.depends
	/usr/bin/ocamlc -c -dtypes -I tests -o parser.cmo parser.ml
	/usr/bin/ocamlc env.cmo syntax.cmo compileHelpers.cmo semantics.cmo parser.cmo lexer.cmo main.cmo -o main.byte
	

runtime.s:
	clang -c -S -emit-llvm runtime.c

runtime.s.bc: runtime.s
	llvm-as-3.1 runtime.s

.PHONY: clean

clean:
	rm -rf *.s.bc *.s main.byte *.cmo *.cmi *.ml.depends *.mli.depends *.annot lexer.ml parser.ml parser.mli *.out *.out.bc
 
zip:
	zip ../CoreCompiler.zip *.{hs,x,y,c,h} Makefile
