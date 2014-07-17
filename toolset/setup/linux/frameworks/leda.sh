#!/bin/bash

RETCODE=$(fw_exists leda-0.5.0)
[ ! "$RETCODE" == 0 ] || { return 0; }

sudo apt-get install -y --fix-missing g++
sudo apt-get install -y --fix-missing luarocks

sudo luarocks install lua-cjson

wget http://sergeyzavadski.github.io/leda/releases/leda-0.5.0/leda-0.5.0.tar.gz
tar xvf leda-0.5.0.tar.gz
cd leda-0.5.0

./configure
make 
sudo make install
