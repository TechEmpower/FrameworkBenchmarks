#!/bin/bash

fw_depends lua

sudo luarocks install lua-cjson

wget -O leda.tar.gz http://sergeyzavadski.github.io/leda/releases/leda-0.5.4/leda-0.5.4.tar.gz 
fw_untar leda.tar.gz
cd leda-0.5.4

./configure
make 
sudo make install
cd .. 
mv leda-0.5.4 leda
