#!/bin/bash

VERSION="2.1.1"
VERTX_HOME=$IROOT/vert.x-$VERSION
RETCODE=$(fw_exists ${VERTX_HOME}.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $VERTX_HOME.installed
  return 0; }

fw_get http://dl.bintray.com/vertx/downloads/vert.x-2.1.1.tar.gz?direct=true -O vert.x-2.1.1.tar.gz
fw_untar vert.x-2.1.1.tar.gz

echo "export VERTX_HOME=${VERTX_HOME}" > $VERTX_HOME.installed
echo -e "export PATH=${VERTX_HOME}/bin:\$PATH" >> $VERTX_HOME.installed

source $VERTX_HOME.installed
