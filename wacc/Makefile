# bin_annot is required for Merlin and other IDE-like tools

# build a local version of menhir since it is NA on HXLY machines
OCB_FLAGS = -use-ocamlfind -I src -use-menhir -yaccflags --dump,--graph,--explain
PATH := $(PWD)/../menhir-20170712/build/bin:$(PATH)
WACCLIB := $(PWD)/../wacclib.s

OBJDIR := _build
VPATH = src

ifdef DEBUG
OCB_FLAGS = -I src -use-menhir -yaccflags --dump,--graph,--trace,--explain
endif

OCB = ocamlbuild $(OCB_FLAGS)

%.byte: %.ml
	$(OCB) $@

%.native: %.ml
	$(OCB) $@

clean:
	$(OCB) -clean

wacclib.s:
	$(PWD)/../tools/arm-gcc -S src/wacclib.c -o wacclib.s

test:
	ocamlbuild -I src -use-ocamlfind tests/test_main.byte && ./test_main.byte

.PHONY: clean wacclib test
