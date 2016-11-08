#!/bin/bash

fw_depends java resin maven

sed -i 's|127.0.0.1|'${DBHOST}'|g' src/main/webapp/WEB-INF/resin-web.xml
sed -i 's|localhost|'${DBHOST}'|g' src/main/java/hellowicket/WicketApplication.java

mvn clean compile war:war
rm -rf $RESIN_HOME/webapps/*
cp target/hellowicket-1.0-SNAPSHOT.war $RESIN_HOME/webapps/wicket.war
resinctl start
