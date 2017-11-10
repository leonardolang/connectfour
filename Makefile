all:
	ocamlfind ocamlc -package owl -linkpkg -o conn4 conn4.ml

clean:
	rm -f *.cm* conn4