#!/bin/bash

rm -f *.o
cp -R $IROOT/onion onion/onion
rm CMakeCache.txt
make && ./hello