#!/bin/bash
export JAVA_HOME=/usr/lib/jvm/java-1.7.0-openjdk-amd64
export RESIN_HOME=${IROOT}/resin-4.0.41

sed -i 's|localhost|'"${DBHOST}"'|g' src/main/webapp/WEB-INF/resin-web.xml
sed -i 's|localhost|'"${DBHOST}"'|g' src/main/java/app/config/DbConfig.java

mvn clean package
rm -rf $RESIN_HOME/webapps/*
cp target/activeweb.war $RESIN_HOME/webapps/
$RESIN_HOME/bin/resinctl start