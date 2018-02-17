#!/bin/bash

fw_installed py3 && return 0
  
PY3_ROOT=$IROOT/py3
PY3_VERSION=3.6.4

install_from_source() {

    fw_get -O http://www.python.org/ftp/python/${PY3_VERSION}/Python-${PY3_VERSION}.tar.xz
    fw_untar Python-${PY3_VERSION}.tar.xz
    cd Python-${PY3_VERSION}
    ./configure --prefix=$PY3_ROOT --disable-shared --with-computed-gotos --quiet --enable-optimizations --with-lto
    # --enable-optimizations enables Profile-guided optimization (PGO) https://bugs.python.org/issue26359
    # --with-lto enables Link Time Optimizations (LTO) support for GCC and Clang https://bugs.python.org/issue25702
    make -j4 --quiet 2>&1 | tee $IROOT/python3-install.log | awk '{ if (NR%100 == 0) printf "."}'
    make install --quiet 2>&1 | tee -a $IROOT/python3-install.log | awk '{ if (NR%100 == 0) printf "."}'
    cd ..
}

install_from_ppa() {
    # ubuntu 14.04 & 16.04 supported https://launchpad.net/~deadsnakes/+archive/ubuntu/ppa
    sudo add-apt-repository --yes ppa:deadsnakes/ppa
    sudo apt-get update
    sudo apt-get install -yqq python3.6 python3.6-dev python3.6-venv
}

install_from_ppa

sudo /usr/bin/python3.6 -m ensurepip -U
sudo /usr/bin/python3.6 -m pip install -U pip
sudo /usr/bin/python3.6 -m pip install -U setuptools wheel

touch $IROOT/py3.installed
