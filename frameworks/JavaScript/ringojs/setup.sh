#!/bin/bash

sed -i 's|dbHost = \x27.*\x27;|dbHost = \x27'"${DBHOST}"'\x27|g' ringo-main.js

fw_depends java ringojs

rm -rf $RINGOJS_HOME/packages/*
ringo-admin install oberhamsi/sql-ringojs-client
ringo --production -J-server -J-Xmx1g -J-Xms1g ringo-main.js &
