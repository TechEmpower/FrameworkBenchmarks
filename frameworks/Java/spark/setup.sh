#!/bin/bash

# load java environment variables
source $IROOT/java7.installed

export RESIN_HOME=${IROOT}/resin-4.0.41

sed -i 's|mysql://.*:3306|mysql://'"${DBHOST}"':3306|g' src/main/webapp/WEB-INF/resin-web.xml

mvn clean package -Ddb-host=${DBHOST}
rm -rf $RESIN_HOME/webapps/*
cp target/spark.war $RESIN_HOME/webapps/spark.war
$RESIN_HOME/bin/resinctl start