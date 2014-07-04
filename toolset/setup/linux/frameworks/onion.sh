#!/bin/bash

fw_exists onion
[ $? -ne 0 ] || { return 0; }

git clone https://github.com/davidmoreno/onion.git


cd onion
mkdir build
cd build
cmake ..
make