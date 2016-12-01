#!/bin/bash

fw_depends java resin maven

sed -i 's|localhost|'${DBHOST}'|g' src/main/java/hellowicket/WicketApplication.java

mvn clean compile war:war
rm -rf $RESIN_HOME/webapps/*
cp target/hellowicket-*.war $RESIN_HOME/webapps/wicket.war
resinctl start
