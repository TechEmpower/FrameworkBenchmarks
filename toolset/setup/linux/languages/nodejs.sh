#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/node.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/node.installed
  return 0; }

VERSION="0.12.12"

fw_get -O http://nodejs.org/dist/v$VERSION/node-v$VERSION-linux-x64.tar.gz
fw_untar node-v$VERSION-linux-x64.tar.gz

NODE_HOME=$IROOT/node-v$VERSION-linux-x64
# Upgrade npm to avoid https://github.com/npm/npm/issues/4984
$NODE_HOME/bin/npm install -g npm

echo "export NODE_ENV=production" > $IROOT/node.installed
echo "export NODE_HOME=${NODE_HOME}" >> $IROOT/node.installed
echo -e "export PATH=\$NODE_HOME/bin:\$PATH" >> $IROOT/node.installed

source $IROOT/node.installed
