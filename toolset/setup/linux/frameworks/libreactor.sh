#!/bin/bash

fw_depends gcc-4.9

fw_installed libreactor && return 0

LIBREACTOR_HOME=$IROOT/libreactor_techempower

git clone https://github.com/fredrikwidlund/libreactor_techempower
cd $LIBREACTOR_HOME
# 4/14/2017
git checkout 675ccca0fb74a5161f8c0eae806d9a275b63a50c
export CC=gcc-4.9
export RANLIB=gcc-ranlib-4.9
export AR=gcc-ar-4.9
./autogen.sh
./configure
make

echo "export LIBREACTOR_HOME=${LIBREACTOR_HOME}" >> $IROOT/libreactor.installed
echo -e "export PATH=\$LIBREACTOR_HOME/:\$PATH" >> $IROOT/libreactor.installed

source $IROOT/libreactor.installed
