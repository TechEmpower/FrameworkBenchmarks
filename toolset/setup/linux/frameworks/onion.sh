#!/bin/bash

RETCODE=$(fw_exists onion)
[ ! "$RETCODE" == 0 ] || { return 0; }

git clone https://github.com/davidmoreno/onion.git


cd onion
git checkout c460557bfc7d45fb6ba61cb6b7259480a67dde82
mkdir -p build
cd build
cmake ..
make
