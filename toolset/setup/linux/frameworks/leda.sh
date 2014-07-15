#!/bin/bash

RETCODE=$(fw_exists leda)
[ ! "$RETCODE" == 0 ] || { return 0; }

sudo apt-get install g++

git clone git@github.com:sergeyzavadski/leda.git
cd leda
./configure
make 
sudo make install
