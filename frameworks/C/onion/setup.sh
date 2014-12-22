#!/bin/bash

export ONION_LOG=noinfo

rm -f *.o
ln -s $IROOT/onion onion
rm -f onion/build/CMakeCache.txt
make
./hello &