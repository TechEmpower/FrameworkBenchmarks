#!/bin/bash

VERSION=0.23
DUDA=$IROOT/duda-$VERSION
RETCODE=$(fw_exists ${DUDA}.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $DUDA.installed
  return 0; }

fw_get -O http://duda.io/releases/duda-client/dudac-$VERSION.tar.gz
fw_untar dudac-$VERSION.tar.gz

DUDA_HOME=$IROOT/dudac-$VERSION
cd $DUDA_HOME

./dudac -r
./dudac -s

echo "export DUDA_HOME=${DUDA_HOME}" > $DUDA.installed
echo "export PATH=${DUDA_HOME}:$PATH" >> $DUDA.installed

source $DUDA.installed
