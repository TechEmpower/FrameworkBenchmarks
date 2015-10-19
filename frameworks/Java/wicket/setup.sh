#!/bin/bash

fw_depends java7 resin maven

sed -i 's|127.0.0.1|'${DBHOST}'|g' src/main/webapp/WEB-INF/resin-web.xml

mvn clean compile war:war
rm -rf $RESIN_HOME/webapps/*
cp target/hellowicket-1.0-SNAPSHOT.war $RESIN_HOME/webapps/wicket.war
resinctl start
