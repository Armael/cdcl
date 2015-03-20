all:
	ocamlbuild -I src -package unix -cflag -unsafe sudoku.native
	ocamlbuild -I src -package unix -cflag -unsafe main.native
	mv sudoku.native sudoku-solver

debug:
	ocamlbuild -I src -package unix -tag debug sudoku.native
	ocamlbuild -I src -package unix -tag debug main.native
	mv sudoku.native sudoku-solver

clean:
	ocamlbuild -clean
