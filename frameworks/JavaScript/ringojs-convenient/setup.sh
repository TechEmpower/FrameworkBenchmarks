#!/bin/bash

sed -i 's|dbHost = \x27.*\x27;|dbHost = \x27'"${DBHOST}"'\x27|g' app/models.js

sudo rm -rf /usr/share/ringojs/packages/*
sudo ringo-admin install oberhamsi/sql-ringojs-client
sudo ringo-admin install grob/ringo-sqlstore
sudo ringo-admin install ringo/stick
sudo ringo-admin install oberhamsi/reinhardt

sudo mkdir -p /usr/share/ringojs/packages/ringo-sqlstore/jars/
sudo cp /usr/share/ringojs//packages/sql-ringojs-client/jars/mysql.jar /usr/share/ringojs/packages/ringo-sqlstore/jars/
ringo --production -J-server -J-Xmx1g -J-Xms1g ringo-main.js &