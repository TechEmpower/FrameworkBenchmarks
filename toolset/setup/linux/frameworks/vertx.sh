#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/vertx.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/vertx.installed
  return 0; }

VERSION="2.1.5"
FREEMAKER_VERSION="2.3.22"
VERTX_HOME=$IROOT/vert.x-$VERSION

fw_get -o vert.x-${VERSION}.tar.gz http://dl.bintray.com/vertx/downloads/vert.x-${VERSION}.tar.gz?direct=true
fw_untar vert.x-${VERSION}.tar.gz
fw_get -o $IROOT/vert.x-${VERSION}/lib/freemarker-${FREEMAKER_VERSION}.jar http://central.maven.org/maven2/org/freemarker/freemarker/${FREEMAKER_VERSION}/freemarker-${FREEMAKER_VERSION}.jar

echo "export VERTX_HOME=${VERTX_HOME}" > $IROOT/vertx.installed
echo -e "export PATH=\$VERTX_HOME/bin:\$PATH" >> $IROOT/vertx.installed

source $IROOT/vertx.installed
