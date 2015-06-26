#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/zeromq.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/zeromq.installed
  return 0; }

ZMQ_VERSION="4.0.3"

fw_get -O http://download.zeromq.org/zeromq-$ZMQ_VERSION.tar.gz
fw_untar zeromq-$ZMQ_VERSION.tar.gz
mv zeromq-$ZMQ_VERSION zeromq-$ZMQ_VERSION-install
cd zeromq-$ZMQ_VERSION-install
./configure --prefix=${IROOT}/zeromq-$ZMQ_VERSION
make
make install

echo "" > $IROOT/zeromq.installed

source $IROOT/zeromq.installed
