#!/bin/bash

fw_depends onion

make clean
ln -s $IROOT/onion onion
rm -f onion/build/CMakeCache.txt
make
./hello &
