#!/bin/bash

sed -i 's|dbHost = \x27.*\x27;|dbHost = \x27'"${DBHOST}"'\x27|g' app/models.js

fw_depends java ringojs

rm -rf $RINGOJS_HOME/packages/*
ringo-admin install oberhamsi/sql-ringojs-client
ringo-admin install grob/ringo-sqlstore
ringo-admin install ringo/stick
ringo-admin install oberhamsi/reinhardt

mkdir -p $RINGOJS_HOME/packages/ringo-sqlstore/jars/
cp $RINGOJS_HOME/packages/sql-ringojs-client/jars/mysql.jar $RINGOJS_HOME/packages/ringo-sqlstore/jars/

ringo --production -J-server -J-Xmx1g -J-Xms1g ringo-main.js &
