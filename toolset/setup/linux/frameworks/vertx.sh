#!/bin/bash

VERSION="2.1.5"
FREEMAKER_VERSION="2.3.22"
VERTX_HOME=$IROOT/vert.x-$VERSION
RETCODE=$(fw_exists ${VERTX_HOME}.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $VERTX_HOME.installed
  return 0; }

fw_get http://dl.bintray.com/vertx/downloads/vert.x-${VERSION}.tar.gz?direct=true -O vert.x-${VERSION}.tar.gz
fw_untar vert.x-${VERSION}.tar.gz
fw_get http://central.maven.org/maven2/org/freemarker/freemarker/${FREEMAKER_VERSION}/freemarker-${FREEMAKER_VERSION}.jar -o $IROOT/vert.x-${VERSION}/lib/freemarker-${FREEMAKER_VERSION}.jar

echo "export VERTX_HOME=${VERTX_HOME}" > $VERTX_HOME.installed
echo -e "export PATH=${VERTX_HOME}/bin:\$PATH" >> $VERTX_HOME.installed

source $VERTX_HOME.installed
