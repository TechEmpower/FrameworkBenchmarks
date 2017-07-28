#!/bin/bash

OCAML_VER='4.03.0'
MIRAGE_VER='3.0.5'

UBUNTU_VER=$(lsb_release -sr | cut -d'.' -f1)
expr $UBUNTU_VER

echo "Installing Dependencies"
sudo apt-get update
sudo apt-get install -yqq m4

echo "Installing OPAM (takes >10min)"
if (( $UBUNTU_VER >= 16 )); then  # Travis Ubuntu 14.04 support
    sudo apt-get install -yqq opam
else
    sudo add-apt-repository -y ppa:avsm/ppa
    sudo apt-get update
    sudo apt-get install -yqq ocaml ocaml-native-compilers camlp4-extra opam
fi
opam init --auto-setup
eval $(opam config env)

echo "Switching to OCaml Compiler $OCAML_VER"
opam switch $OCAML_VER

echo "    Setting up compiler env"
eval `opam config env`

echo "Installing MirageOS $MIRAGE_VER"
opam install -y mirage=$MIRAGE_VER

echo "Installing cohttp dependencies"
opam install -y cohttp lwt js_of_ocaml cohttp-lwt-unix

echo "Building MirageOS HTTP Server"
ocamlbuild -pkg cohttp-lwt-unix server_example.native

echo "Booting MirageOS Conduit HTTP Server"
./server_example.native
