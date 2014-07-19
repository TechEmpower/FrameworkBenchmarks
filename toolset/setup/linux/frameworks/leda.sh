#!/bin/bash

RETCODE=$(fw_exists leda)
[ ! "$RETCODE" == 0 ] || { return 0; }

rm -rf leda || true

sudo apt-get install -y  g++
sudo apt-get install -y  luarocks

sudo luarocks install lua-cjson


wget -O leda.tar.gz http://sergeyzavadski.github.io/leda/releases/leda-0.5.3/leda-0.5.3.tar.gz 
fw_untar leda.tar.gz
cd leda-0.5.3

./configure
make 
sudo make install
cd .. 
mv leda-0.5.3 leda

