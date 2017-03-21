#!/bin/bash

fw_depends mongodb nodejs

sed -i 's|mongodb://.*/hello_world|mongodb://'"${DBHOST}"'/hello_world|g' app.js

# run app
npm install
node --harmony app &
