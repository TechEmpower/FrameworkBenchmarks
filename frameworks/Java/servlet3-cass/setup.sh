#!/bin/bash

sed -i 's|localhost|'"${DBHOST}"'|g' src/main/resources/application.properties

fw_depends java resin maven

mvn clean compile war:war
rm -rf $RESIN_HOME/webapps/*
cp target/servlet3-cass.war $RESIN_HOME/webapps
$RESIN_HOME/bin/resinctl start
