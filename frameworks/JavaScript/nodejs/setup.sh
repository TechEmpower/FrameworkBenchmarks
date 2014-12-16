#!/bin/bash

sed -i 's|localhost|'"${DBHOST}"'|g' hello.js
sed -i 's|mongodb//.*/hello_world|mongodb//'"${DBHOST}"'/hello_world|g' hello.js

export NODE_ENV=production

${NODE_HOME}/bin/npm install
${NODE_HOME}/bin/node hello.js &