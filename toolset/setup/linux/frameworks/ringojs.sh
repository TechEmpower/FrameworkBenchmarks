#!/bin/bash

fw_installed ringojs && return 0

VERSION="1.1.0"
RINGOJS=$IROOT/ringojs_$VERSION
RINGOJS_HOME=$IROOT/ringojs-$VERSION

fw_get -O https://github.com/ringo/ringojs/releases/download/v$VERSION/ringojs-$VERSION.tar.gz
fw_untar ringojs-$VERSION.tar.gz

echo "export RINGOJS_HOME=${RINGOJS_HOME}" > $IROOT/ringojs.installed
echo -e "export PATH=\$RINGOJS_HOME/bin:\$PATH" >> $IROOT/ringojs.installed

source $IROOT/ringojs.installed
