# Sample Makefile for the WACC Compiler lab: edit this to build your own comiler
# Locations

# Tools

FIND	:= find
RM	:= rm -rf
MKDIR	:= mkdir -p

# the make rules

all: build_deps
	make -C wacc driver.native

build_deps:
	chmod u+x build.sh && ./build.sh

# runs the antlr build script then attempts to compile all .java files within src

clean:
	make -C wacc clean

wacclib.s:
	./tools/arm-gcc -S wacc/src/wacclib.c -o wacclib.s

.PHONY: all rules clean wacclib.s
