#!/bin/bash

fw_depends nvm nodejs

sed -i 's|localhost|'"${DBHOST}"'|g' hello.js
sed -i 's|mongodb://.*/hello_world|mongodb://'"${DBHOST}"'/hello_world|g' hello.js

npm install
node app.js &
