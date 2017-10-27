# Sample Makefile for the WACC Compiler lab: edit this to build your own comiler
# Locations

ANTLR_DIR	:= antlr_config
SOURCE_DIR	:= src
OUTPUT_DIR	:= bin 

# Tools

ANTLR	:= antlrBuild
FIND	:= find
RM	:= rm -rf
MKDIR	:= mkdir -p
JAVA	:= java
JAVAC	:= javac

JFLAGS	:= -sourcepath $(SOURCE_DIR) -d $(OUTPUT_DIR) -cp lib/antlr-4.4-complete.jar 

# the make rules

all: build_deps
	cd src/legacy && make driver.native

build_deps:
	chmod u+x build.sh && ./build.sh

# runs the antlr build script then attempts to compile all .java files within src
rules:
	cd $(ANTLR_DIR) && ./$(ANTLR) 
	$(FIND) $(SOURCE_DIR) -name '*.java' > $@
	$(MKDIR) $(OUTPUT_DIR)
	$(JAVAC) $(JFLAGS) @$@
	$(RM) rules

clean:
	$(RM) rules $(OUTPUT_DIR) && rm -rf src/legacy/lib/menhir-20170712/build/ && rm -rf src/legacy/_build

.PHONY: all rules clean

download_wacc_examples:
	git clone git@gitlab.doc.ic.ac.uk:lab1718_autumn/wacc_examples.git test/wacc_examples
