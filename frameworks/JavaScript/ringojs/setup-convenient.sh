#!/bin/bash

fw_depends mysql java ringojs

rm -rf $RINGOJS_HOME/packages/*
ringo-admin install grob/ringo-sqlstore
ringo-admin install ringo/stick
ringo-admin install orfon/reinhardt

(cd $RINGOJS_HOME/packages/ringo-sqlstore/jars && curl -s -O https://repo1.maven.org/maven2/mysql/mysql-connector-java/5.1.39/mysql-connector-java-5.1.39.jar)

ringo --production -J-server -J-Xmx1g -J-Xms1g ringo-convenient-main.js --host 0.0.0.0
