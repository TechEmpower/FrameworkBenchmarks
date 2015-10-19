#!/bin/bash

sed -i 's|localhost|'"${DBHOST}"'|g' src/main/webapp/WEB-INF/resin-web.xml
sed -i 's|localhost|'"${DBHOST}"'|g' src/main/java/app/config/DbConfig.java

fw_depends java7 resin maven

mvn clean package
rm -rf $RESIN_HOME/webapps/*
cp target/activeweb.war $RESIN_HOME/webapps/
resinctl start
