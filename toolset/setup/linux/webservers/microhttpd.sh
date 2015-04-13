#!/bin/bash

VERSION=0.9.39
MICROHTTPD=$IROOT/libmicrohttpd
MICROHTTPD_HOME=$MICROHTTPD-$VERSION
RETCODE=$(fw_exists ${MICROHTTPD}.installed)
[ ! "$RETCODE" == 0 ] || { \
  # Load environment variables
  source $MICROHTTPD.installed
  return 0; }

fw_get http://mirror.ibcp.fr/pub/gnu/libmicrohttpd/libmicrohttpd-$VERSION.tar.gz
fw_untar libmicrohttpd-$VERSION.tar.gz
cd libmicrohttpd-$VERSION
./configure --prefix=$IROOT
make install

echo "" > $MICROHTTPD.installed

source $MICROHTTPD.installed
