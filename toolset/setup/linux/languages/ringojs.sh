#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/ringojs.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/ringojs.installed
  return 0; }

VERSION="0.11"
RINGOJS=$IROOT/ringojs_$VERSION
RINGOJS_HOME=$IROOT/ringojs-$VERSION

fw_get -O https://github.com/ringo/ringojs/releases/download/v$VERSION.0/ringojs-$VERSION.tar.gz
fw_untar ringojs-$VERSION.tar.gz

echo "export RINGOJS_HOME=${RINGOJS_HOME}" > $IROOT/ringojs.installed
echo -e "export PATH=\$RINGOJS_HOME/bin:\$PATH" >> $IROOT/ringojs.installed

source $IROOT/ringojs.installed
