# bin_annot is required for Merlin and other IDE-like tools

OBJDIR := _build
VPATH = src

OCB_FLAGS = -I src -I src/bin -use-ocamlfind -use-menhir

OCB = ocamlbuild $(OCB_FLAGS)

%.byte: %.ml
	$(OCB) $@

driver.byte:
	$(OCB) src/driver.byte

driver.native:
	$(OCB) src/driver.native

clean:
	$(OCB) -clean

wacclib.s:
	$(PWD)/../tools/arm-gcc -S src/wacclib.c -o wacclib.s

test:
	ocamlbuild -I src -use-ocamlfind tests/test_main.byte && ./test_main.byte

.PHONY: clean wacclib test driver.byte driver.native
