#!/bin/bash

sed -i 's|mongodb://.*/hello_world|mongodb://'"${DBHOST}"'/hello_world|g' app.js
sed -i 's|localhost|'"${DBHOST}"'|g' app.js

export NODE_ENV=production
export NODE_HOME=${IROOT}/node-v0.10.8-linux-x64
export PATH=$PATH:$NODE_HOME/bin

${NODE_HOME}/bin/npm install
${NODE_HOME}/bin/node app &

# !DO NOT REMOVE!
#
# It takes `node app` a few seconds to turn on and 
# then fork. If you remove this sleep, the parent shell 
# executing this script will be terminated before the 
# application has time to awaken and be forked, and 
# express will fail to be started
sleep 5