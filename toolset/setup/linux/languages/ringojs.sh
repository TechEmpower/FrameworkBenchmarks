#!/bin/bash

VERSION="0.11"
RINGOJS=$IROOT/ringojs_$VERSION
RINGOJS_HOME=$IROOT/ringojs-$VERSION
RETCODE=$(fw_exists ${RINGOJS}.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $RINGOJS.installed
  return 0; }

fw_get https://github.com/ringo/ringojs/releases/download/v$VERSION.0/ringojs-$VERSION.tar.gz -O
fw_untar ringojs-$VERSION.tar.gz

echo "export RINGOJS_HOME=${RINGOJS_HOME}" > $RINGOJS.installed
echo -e "export PATH=${RINGOJS_HOME}/bin:\$PATH" >> $RINGOJS.installed

source $RINGOJS.installed
