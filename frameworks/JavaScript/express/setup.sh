#!/bin/bash

sed -i 's|mongodb://.*/hello_world|mongodb://'"${DBHOST}"'/hello_world|g' app.js
sed -i 's|localhost|'"${DBHOST}"'|g' app.js

export NODE_ENV=production

${NODE_HOME}/bin/npm install
${NODE_HOME}/bin/node app &