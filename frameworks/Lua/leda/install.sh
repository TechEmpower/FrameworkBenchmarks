#!/bin/bash

fw_depends lua

sudo luarocks install lua-cjson

git clone https://github.com/sergeyzavadski/leda.git
cd leda
git checkout d1d805877c506e34d23d5aac2b27608c192695d3
./configure
make 
sudo make install