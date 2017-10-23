# bin_annot is required for Merlin and other IDE-like tools

# build a local version of menhir since it is NA on HXLY machines
OCB_FLAGS = -use-ocamlfind -I src -use-menhir -yaccflags --dump,--graph,--explain

OBJDIR := _build
VPATH = src

ifdef DEBUG
OCB_FLAGS = -I src -use-menhir -yaccflags --dump,--graph,--trace,--explain
endif

OCB = ocamlbuild $(OCB_FLAGS)

driver.byte: wacclib.o
	$(OCB) driver.byte

driver.native: wacclib.o
	$(OCB) driver.native

test_codegen.byte: wacclib.o
	$(OCB) translate_ast.byte


wacclib.o: wacclib.c
	$(OCB) wacclib.o

clean:
	$(OCB) -clean;

test: driver.byte
	python run_test.py

.PHONY: clean test
