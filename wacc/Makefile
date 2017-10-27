# bin_annot is required for Merlin and other IDE-like tools

# build a local version of menhir since it is NA on HXLY machines
OCB_FLAGS = -use-ocamlfind -I src -use-menhir -yaccflags --dump,--graph,--explain
# PATH := $(PWD)/lib/menhir-20170712/build/bin:$(PATH)

OBJDIR := _build
VPATH = src

ifdef DEBUG
OCB_FLAGS = -I src -use-menhir -yaccflags --dump,--graph,--trace,--explain
endif

OCB = ocamlbuild $(OCB_FLAGS)

%.byte: wacclib.o
	$(OCB) $@

%.native: wacclib.o
	$(OCB) $@

wacclib.o: wacclib.c
	$(OCB) wacclib.o

clean:
	$(OCB) -clean;

.PHONY: clean test
