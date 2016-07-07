#!/bin/bash

sed -i 's|dbHost = \x27.*\x27;|dbHost = \x27'"${DBHOST}"'\x27|g' ringo-main.js

fw_depends java ringojs

rm -rf $RINGOJS_HOME/packages/*
ringo-admin install oberhamsi/sql-ringojs-client
(cd $RINGOJS_HOME/packages/sql-ringojs-client/jars && curl -s -o mysql.jar https://repo1.maven.org/maven2/mysql/mysql-connector-java/5.1.39/mysql-connector-java-5.1.39.jar)

ringo --production -J-server -J-Xmx1g -J-Xms1g ringo-main.js &
