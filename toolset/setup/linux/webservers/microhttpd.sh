#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/microhttpd.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/microhttpd.installed
  return 0; }

VERSION=0.9.39
MICROHTTPD=$IROOT/libmicrohttpd
MICROHTTPD_HOME=$MICROHTTPD-$VERSION

fw_get -O http://mirror.ibcp.fr/pub/gnu/libmicrohttpd/libmicrohttpd-$VERSION.tar.gz
fw_untar libmicrohttpd-$VERSION.tar.gz
cd libmicrohttpd-$VERSION
./configure --prefix=$MICROHTTPD_HOME
make install

echo "export MICROHTTPD_HOME=${MICROHTTPD_HOME}" > $IROOT/microhttpd.installed
echo -e "export PATH=${MICROHTTPD_HOME}/bin:\$PATH" >> $IROOT/microhttpd.installed

source $IROOT/microhttpd.installed
