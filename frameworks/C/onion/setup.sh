#!/bin/bash

export ONION_LOG=noinfo

make clean
ln -s $IROOT/onion onion
rm -f onion/build/CMakeCache.txt
make
./hello &