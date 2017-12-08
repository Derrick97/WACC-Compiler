#!/usr/bin/env bash


if ! [ -x "$(command -v opam)" ]; then
  echo 'Install OPAM now ' >&2
  # Download OPAM, install with system compiler
  export OPAMROOT=`pwd`/opamroot
  wget https://raw.github.com/ocaml/opam/latest/shell/opam_installer.sh -O - | sh -s ./ system
  ./opam init --comp system
  eval `./opam config env`
  export PATH=`pwd`:$PATH
else
  echo "OPAM exists locally"
fi
  opam install -y ppx_deriving ocamlgraph ounit menhir

make -C wacc driver.native
