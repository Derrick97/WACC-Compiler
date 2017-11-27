# Sample Makefile for the WACC Compiler lab: edit this to build your own comiler
# Locations

PATH := $(PWD)/menhir-20170712/build/bin:$(PATH)

# Tools

FIND	:= find
RM	:= rm -rf
MKDIR	:= mkdir -p

# the make rules

all: build_deps
	cd wacc/ && make driver.native

build_deps:
	chmod u+x build.sh && ./build.sh

# runs the antlr build script then attempts to compile all .java files within src

clean:
	$(RM) rules $(OUTPUT_DIR) && cd wacc && make clean

wacclib.s:
	./tools/arm-gcc -S wacc/src/wacclib.c -o wacclib.s

.PHONY: all rules clean wacclib.s
