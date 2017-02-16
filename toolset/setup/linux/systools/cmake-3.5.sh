#!/bin/bash

fw_installed cmake-3.5 && return 0

fw_get -O http://www.cmake.org/files/v3.5/cmake-3.5.2.tar.gz
fw_untar cmake-3.5.2.tar.gz
cd cmake-3.5.2
./configure
make
sudo make install
sudo update-alternatives --install /usr/bin/cmake cmake /usr/local/bin/cmake 1 --force

echo -e "" >> $IROOT/cmake-3.5.installed

source $IROOT/cmake-3.5.installed
