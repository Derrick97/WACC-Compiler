#!/usr/bin/env bash


if ! [ -x "$(command -v opam)" ]; then
  echo 'Install OPAM now ' >&2
  # Download OPAM, install with system compiler
  wget https://raw.github.com/ocaml/opam/master/shell/opam_installer.sh -O - | sh -s ./ system
  export OPAMROOT=`pwd`/opamroot
  eval `./opam config env`
  export PATH=`pwd`:$PATH
else
  echo "OPAM exists locally"
fi
