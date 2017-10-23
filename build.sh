#!/usr/bin/env bash
cd src/legacy/lib/menhir-20170712/
mkdir -p `pwd`/build
make PREFIX=`pwd`/build USE_OCAMLFIND=false all
make PREFIX=`pwd`/build USE_OCAMLFIND=false install
