#!/bin/bash

VERSION="0.11"
RINGOJS=$IROOT/ringojs_$VERSION
RINGOJS_HOME=$IROOT/ringojs-$VERSION
RETCODE=$(fw_exists ${RINGOJS}.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $RINGOJS.installed
  return 0; }

fw_get http://ringojs.org/downloads/ringojs-0.11.tar.gz
fw_untar ringojs-0.11.tar.gz

echo "export RINGOJS_HOME=${RINGOJS_HOME}" > $RINGOJS.installed
echo -e "export PATH=${RINGOJS_HOME}/bin:\$PATH" >> $RINGOJS.installed

source $RINGOJS.installed
