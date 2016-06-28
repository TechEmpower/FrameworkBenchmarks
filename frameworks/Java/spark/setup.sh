#!/bin/bash

sed -i 's|mysql://.*:3306|mysql://'"${DBHOST}"':3306|g' src/main/webapp/WEB-INF/resin-web.xml

fw_depends java resin maven

mvn clean package -Ddb-host=${DBHOST}
rm -rf $RESIN_HOME/webapps/*
cp target/spark.war $RESIN_HOME/webapps/spark.war
resinctl start
