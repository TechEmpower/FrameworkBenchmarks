#!/bin/bash

RETCODE=$(fw_exists onion)
[ ! "$RETCODE" == 0 ] || { return 0; }

git clone https://github.com/davidmoreno/onion.git


cd onion
mkdir -p build
cd build
cmake ..
make