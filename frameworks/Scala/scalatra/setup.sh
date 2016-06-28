#!/bin/bash

fw_depends java resin sbt

sed -i 's|mysql://.*:3306|mysql://'"${DBHOST}"':3306|g' src/main/webapp/WEB-INF/resin-web.xml

sbt clean package -batch

rm -rf $RESIN_HOME/webapps/*
cp target/scala-2.10/scalatra*.war $RESIN_HOME/webapps/scalatra.war

resinctl start
