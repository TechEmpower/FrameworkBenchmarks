#!/bin/bash

fw_depends nodejs

sed -i 's|mongodb://.*/hello_world|mongodb://'"${DBHOST}"'/hello_world|g' app.js
sed -i 's|localhost|'"${DBHOST}"'|g' app.js

# install dependencies
npm install
# run app
node app &
