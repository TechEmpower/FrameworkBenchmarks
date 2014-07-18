#!/bin/bash

RETCODE=$(fw_exists leda-0.5.2)
[ ! "$RETCODE" == 0 ] || { return 0; }

sudo apt-get install -y  g++
sudo apt-get install -y  luarocks

sudo luarocks install lua-cjson

sudo rm -rf /usr/local/include/event2
sudo rm -rf /usr/include/event2

wget http://sergeyzavadski.github.io/leda/releases/leda-0.5.2/leda-0.5.2.tar.gz
tar xvf leda-0.5.2.tar.gz
cd leda-0.5.2

./configure
make 
sudo make install
