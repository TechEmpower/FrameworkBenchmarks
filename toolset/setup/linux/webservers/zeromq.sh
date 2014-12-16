#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/zeromq-4.0.3.installed)
[ ! "$RETCODE" == 0 ] || { return 0; }

fw_get http://download.zeromq.org/zeromq-4.0.3.tar.gz
fw_untar zeromq-4.0.3.tar.gz
mv zeromq-4.0.3 zeromq-4.0.3-install
cd zeromq-4.0.3-install
./configure --prefix=${IROOT}/zeromq-4.0.3
make
make install

touch ${IROOT}/zeromq-4.0.3.installed