#!/bin/bash

fw_depends gcc-6

fw_installed libreactor && return 0

mkdir -p ${CROOT} || true
cd ${CROOT}

LIBREACTOR_HOME=$IROOT/libreactor
mkdir -p $LIBREACTOR_HOME

cd $LIBREACTOR_HOME
wget https://github.com/fredrikwidlund/libdynamic/releases/download/v1.1.0/libdynamic-1.1.0.tar.gz
tar xfz libdynamic-1.1.0.tar.gz
cd libdynamic-1.1.0
./configure CC=gcc-6 AR=gcc-ar-6 NM=gcc-nm-6 RANLIB=gcc-ranlib-6
make && sudo make install

cd $LIBREACTOR_HOME
wget https://github.com/fredrikwidlund/libreactor/releases/download/v1.0.0/libreactor-1.0.0.tar.gz
tar xfz libreactor-1.0.0.tar.gz
cd libreactor-1.0.0
./configure CC=gcc-6 AR=gcc-ar-6 NM=gcc-nm-6 RANLIB=gcc-ranlib-6
make && sudo make install

cd $LIBREACTOR_HOME
wget https://github.com/fredrikwidlund/libclo/releases/download/v0.1.0/libclo-0.1.0.tar.gz
tar xfz libclo-0.1.0.tar.gz
cd libclo-0.1.0
./configure CC=gcc-6 AR=gcc-ar-6 NM=gcc-nm-6 RANLIB=gcc-ranlib-6
make && sudo make install

cd $LIBREACTOR_HOME
echo "export LIBREACTOR_HOME=${LIBREACTOR_HOME}" >> $IROOT/libreactor.installed

source $IROOT/libreactor.installed
