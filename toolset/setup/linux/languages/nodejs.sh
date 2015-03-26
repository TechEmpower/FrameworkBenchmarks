#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/node-v0.12.0.installed)
[ ! "$RETCODE" == 0 ] || { return 0; }

fw_get http://nodejs.org/dist/v0.12.0/node-v0.12.0-linux-x64.tar.gz
fw_untar node-v0.12.0-linux-x64.tar.gz

# Upgrade npm to avoid https://github.com/npm/npm/issues/4984
export NODE_HOME=${IROOT}/node-v0.12.0-linux-x64
export PATH=$PATH:$NODE_HOME/bin

${NODE_HOME}/bin/npm install -g npm
${NODE_HOME}/bin/npm update

touch ${IROOT}/node-v0.12.0.installed