#!/bin/bash

sed -i 's|mysql://.*:3306|mysql://'"${DBHOST}"':3306|g' hello/src/main/webapp/WEB-INF/resin-web.xml

fw_depends java7 resin maven

cd hello
mvn clean compile war:war
rm -rf $RESIN_HOME/webapps/*
cp target/tapestry.war $RESIN_HOME/webapps/tapestry.war
resinctl start
