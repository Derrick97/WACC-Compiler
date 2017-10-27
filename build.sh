#!/usr/bin/env bash
cd wacc/lib/menhir-20170712/
mkdir -p `pwd`/build
make PREFIX=`pwd`/build USE_OCAMLFIND=false all > menhir_build.out
make PREFIX=`pwd`/build USE_OCAMLFIND=false install >> menhir_build.out

