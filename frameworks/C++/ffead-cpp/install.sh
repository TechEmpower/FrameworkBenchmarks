#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/mongocdriver.installed)
[ ! "$RETCODE" == 0 ] || { return 0; }

sudo apt-get install -y uuid-dev unixodbc unixodbc-dev

wget https://github.com/mongodb/mongo-c-driver/releases/download/1.1.10/mongo-c-driver-1.1.10.tar.gz
tar -xzf mongo-c-driver-1.1.10.tar.gz
cd mongo-c-driver-1.1.10/
./configure --prefix=${IROOT} --libdir=${IROOT}
make && sudo make install
touch ${IROOT}/mongocdriver.installed