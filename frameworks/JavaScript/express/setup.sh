#!/bin/bash

fw_depends nvm nodejs

sed -i 's|mongodb://.*/hello_world|mongodb://'"${DBHOST}"'/hello_world|g' app.js
sed -i 's|localhost|'"${DBHOST}"'|g' app.js

# run app
npm install
node app &
