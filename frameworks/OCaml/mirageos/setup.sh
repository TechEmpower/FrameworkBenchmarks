#!/bin/bash

OCAML_VER='4.03.0'
MIRAGE_VER='3.0.5'

UBUNTU_VER=$(lsb_release -sr | cut -d'.' -f1)
expr $UBUNTU_VER

echo "Installing Dependencies"
sudo apt-get update
sudo apt-get install -yqq m4 qemu-system-x86

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

echo "Building MirageOS HTTP Server"
mirage configure -t virtio --dhcp false
make depend
make

sudo ip tuntap add tap0 mode tap
sudo ip addr add 10.0.0.1/24 dev tap0
sudo ip link set dev tap0 up

echo "Booting MirageOS Conduit HTTP Server"
# The qemu command below is the output of '$ solo5-run-virtio -n tap0 ./conduit_server.virtio' with the addition of '-daemonize' so as to detach the qemu process (& detach will not work)
qemu-system-x86_64 -daemonize -cpu Westmere -m 128 -nodefaults -no-acpi -display none -device virtio-net,netdev=n0 -netdev tap,id=n0,ifname=tap0,script=no,downscript=no -device isa-debug-exit -kernel conduit_server.virtio

# FIXME
# * Need to change TFB-Server to 10.0.0.2 in /etc/hosts, hostfwd doesn't seem to work with qemu tap network config
# *