#!/bin/bash

VERSION="0.13.8"
RETCODE=$(fw_exists ${IROOT}/sbt.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/sbt.installed
  return 0; }

fw_get http://dl.bintray.com/sbt/native-packages/sbt/$VERSION/sbt-$VERSION.zip -O sbt-$VERSION.zip
fw_unzip sbt-$VERSION.zip

echo -e "export PATH=${IROOT}/sbt/bin:\$PATH" > $IROOT/sbt.installed

source $IROOT/sbt.installed
