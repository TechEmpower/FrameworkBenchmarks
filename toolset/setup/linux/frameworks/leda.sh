#!/bin/bash

RETCODE=$(fw_exists leda-0.5.1)
[ ! "$RETCODE" == 0 ] || { return 0; }

sudo apt-get install -y  g++
sudo apt-get install -y  luarocks

sudo luarocks install lua-cjson

wget http://sergeyzavadski.github.io/leda/releases/leda-0.5.1/leda-0.5.1.tar.gz
tar xvf leda-0.5.1.tar.gz
cd leda-0.5.1

./configure
make 
sudo make install
