#!/bin/bash

rm -f *.o
ln -s $IROOT/onion onion
rm -f onion/build/CMakeCache.txt
make
./hello &