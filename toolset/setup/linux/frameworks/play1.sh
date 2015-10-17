#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/play1.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/play1.installed
  return 0; }

VERSION="1.2.5"
PLAY1_HOME=$IROOT/play-$VERSION

fw_get -O http://downloads.typesafe.com/releases/play-$VERSION.zip
fw_unzip play-$VERSION.zip

echo "export PLAY1_HOME=${PLAY1_HOME}" > $IROOT/play1.installed
echo -e "export PATH=\$PLAY1_HOME:\$PATH" >> $IROOT/play1.installed

source $IROOT/play1.installed
