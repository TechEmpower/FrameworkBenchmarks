#!/bin/bash

RETCODE=$(fw_exists /usr/local/lib/libzmq.a)
[ ! "$RETCODE" == 0 ] || { return 0; }

fw_get http://download.zeromq.org/zeromq-4.0.3.tar.gz
fw_untar zeromq-4.0.3.tar.gz
cd zeromq-4.0.3
./configure
make
sudo make install