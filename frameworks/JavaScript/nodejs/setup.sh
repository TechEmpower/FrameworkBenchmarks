#!/bin/bash

fw_depends nodejs

sed -i 's|localhost|'"${DBHOST}"'|g' app.js
sed -i 's|mongodb://.*/hello_world|mongodb://'"${DBHOST}"'/hello_world|g' app.js

npm install
node app.js &
