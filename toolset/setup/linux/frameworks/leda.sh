#!/bin/bash

RETCODE=$(fw_exists leda)
[ ! "$RETCODE" == 0 ] || { return 0; }

wget http://sergeyzavadski.github.io/leda/releases/leda-0.4.1/leda-0.4.1.tar.gz
tar xvf leda-0.4.1.tar.gz
cd leda-0.4.1
./configure
make 
sudo make install
