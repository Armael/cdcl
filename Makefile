all:
	ocamlbuild -package unix sudoku.native

clean:
	ocamlbuild -clean
