#!/bin/sh

apt-get install ocaml opam
apt-get install zlib1g-dev libgmp-dev pkg-config m4
opam init
opam install ocamlfind
opam install cryptokit ounit menhir

