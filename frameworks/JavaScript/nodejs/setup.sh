#!/bin/bash
sed -i 's|localhost|'"${DBHOST}"'|g' hello.js
sed -i 's|mongodb://.*/hello_world|mongodb://'"${DBHOST}"'/hello_world|g' hello.js

export NODE_ENV=production
export NVM_HOME=${IROOT}/nvm
# Used to avoid nvm's return 2 error.
# Sourcing this functions if 0 is returned.
source $NVM_HOME/nvm.sh || 0
nvm install 0.12.2
nvm use 0.12.2

# update npm before app init
npm install -g npm

npm install
node hello.js &