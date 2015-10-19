#!/bin/bash

fw_depends nodejs

sed -i 's|127.0.0.1|'"${DBHOST}"'|g' handlers/mongodb-raw.js
sed -i 's|127.0.0.1|'"${DBHOST}"'|g' handlers/mongoose.js
sed -i 's|127.0.0.1|'"${DBHOST}"'|g' handlers/mysql-raw.js
sed -i 's|127.0.0.1|'"${DBHOST}"'|g' handlers/redis.js
sed -i 's|127.0.0.1|'"${DBHOST}"'|g' handlers/sequelize.js

npm install
node app.js &
