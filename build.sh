#!/usr/bin/env bash

wget http://gallium.inria.fr/~fpottier/menhir/menhir-20170712.tar.gz
tar -xvzf menhir-20170712.tar.gz
cd menhir-20170712/
mkdir -p `pwd`/build
make PREFIX=`pwd`/build USE_OCAMLFIND=false all > menhir_build.out
make PREFIX=`pwd`/build USE_OCAMLFIND=false install >> menhir_build.out
