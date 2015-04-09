#!/bin/bash
# load java environment variables
source $IROOT/java7.installed
export RESIN_HOME=${IROOT}/resin-4.0.41

sed -i 's|mysql://.*:3306|mysql://'"${DBHOST}"':3306|g' hello/src/main/webapp/WEB-INF/resin-web.xml

cd hello
mvn clean compile war:war
rm -rf $RESIN_HOME/webapps/*
cp target/tapestry.war $RESIN_HOME/webapps/tapestry.war
$RESIN_HOME/bin/resinctl start