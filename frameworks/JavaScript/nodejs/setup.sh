#!/bin/bash
sed -i 's|localhost|'"${DBHOST}"'|g' hello.js
sed -i 's|mongodb//.*/hello_world|mongodb//'"${DBHOST}"'/hello_world|g' hello.js

export NODE_ENV=production
export NODE_HOME=${IROOT}/nvm/v0.10.8
export PATH=$PATH:$NODE_HOME/bin

npm install
node hello.js &
# export NVM_HOME=${IROOT}/nvm
# source $NVM_HOME/nvm.sh
# nvm install 0.10.8
# nvm use 0.10.8

# # run app
# npm install
# node app &

# !DO NOT REMOVE!
#
# It takes `node app` a few seconds to turn on and 
# then fork. If you remove this sleep, the parent shell 
# executing this script will be terminated before the 
# application has time to awaken and be forked, and 
# express will fail to be started
sleep 5