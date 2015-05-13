#!/bin/bash

sed -i 's|localhost|'"${DBHOST}"'|g' app.js

fw_depends nvm nodejs

npm install
node app &
