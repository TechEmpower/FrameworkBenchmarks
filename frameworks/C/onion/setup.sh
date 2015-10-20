#!/bin/bash

fw_depends onion

sed -i 's|127.0.0.1|'${DBHOST}'|g' hello.c

make clean
ln -s $IROOT/onion onion
rm -f onion/build/CMakeCache.txt
make
./hello &
