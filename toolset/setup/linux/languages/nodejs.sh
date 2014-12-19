#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/node-v0.10.8.installed)
[ ! "$RETCODE" == 0 ] || { return 0; }

fw_get http://nodejs.org/dist/v0.10.8/node-v0.10.8-linux-x64.tar.gz
fw_untar node-v0.10.8-linux-x64.tar.gz

export PATH="$NODE_HOME/bin:$PATH"

${NODE_HOME}/bin/npm install findup-sync@0.1.2
${NODE_HOME}/bin/npm install -g npm

touch ${IROOT}/node-v0.10.8.installed