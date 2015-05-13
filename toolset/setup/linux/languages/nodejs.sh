#!/bin/bash

VERSION="0.12.2"
RETCODE=$(fw_exists ${IROOT}/node-${VERSION}.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/node-$VERSION.installed
  return 0; }

fw_depends nvm

nvm install 0.12.2

echo "export NODE_ENV=production" > $IROOT/node-$VERSION.installed
echo "nvm use ${VERSION}" >> $IROOT/node-$VERSION.installed
echo "npm install -g npm" >> $IROOT/node-$VERSION.installed

source $IROOT/node-$VERSION.installed
