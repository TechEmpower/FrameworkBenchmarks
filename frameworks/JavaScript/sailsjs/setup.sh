#!/bin/bash

fw_depends nodejs

sed -i 's|127.0.0.1|'"${DBHOST}"'|g' api/controllers/RedisController.js
sed -i 's|127.0.0.1|'"${DBHOST}"'|g' api/controllers/SequelizeMySQLController.js
sed -i 's|127.0.0.1|'"${DBHOST}"'|g' api/controllers/SequelizePostgresController.js

# let us run sails directly
npm install -g sails

# reset cache & run app
npm install
sails lift --port 8080 &
