#!/bin/bash

fw_depends nodejs

sed -i 's|127.0.0.1|'"${DBHOST}"'|g' handlers/mongoose.js
sed -i 's|127.0.0.1|'"${DBHOST}"'|g' handlers/sequelize-postgres.js
sed -i 's|127.0.0.1|'"${DBHOST}"'|g' handlers/sequelize.js
sed -i 's|127.0.0.1|'"${DBHOST}"'|g' handlers/redis.js

npm install
node app &
