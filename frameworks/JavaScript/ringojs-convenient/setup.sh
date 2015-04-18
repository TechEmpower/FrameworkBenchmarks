#!/bin/bash

sed -i 's|dbHost = \x27.*\x27;|dbHost = \x27'"${DBHOST}"'\x27|g' app/models.js

rp install --force-yes
# for license reasons we can not simply package with mysql-jdbc.jar
cp ./packages/sql-ringojs-client/jars/mysql.jar ./packages/ringo-sqlstore/jars/

ringo --production -J-server -J-Xmx1g -J-Xms1g --modules ./packages/ ringo-main.js &