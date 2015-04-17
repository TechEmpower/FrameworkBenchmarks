#!/bin/bash

VERSION="0.10.8"
NODE=$IROOT/node-v$VERSION
NODE_HOME=$NODE-linux-x64
RETCODE=$(fw_exists ${NODE}.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $NODE.installed
  return 0; }

fw_get http://nodejs.org/dist/v$VERSION/node-v$VERSION-linux-x64.tar.gz
fw_untar node-v$VERSION-linux-x64.tar.gz

# Upgrade npm to avoid https://github.com/npm/npm/issues/4984
${NODE_HOME}/bin/npm install -g npm

echo "export NODE_HOME=${NODE_HOME}" > $NODE.installed
echo "export NODE_ENV=production" >> $NODE.installed
echo -e "export PATH=${NODE_HOME}/bin:\$PATH" >> $NODE.installed

source $NODE.installed
