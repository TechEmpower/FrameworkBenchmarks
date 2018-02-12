#!/bin/bash

fw_depends postgresql java8 resin-java8 maven mono dsl_platform

source $IROOT/java.installed

echo "Changing the database"
cat $TROOT/web.xml | sed 's/localhost/'$DBHOST'/g' > $TROOT/src/main/webapp/WEB-INF/web.xml
	
mvn clean compile war:war
rm -rf $RESIN_HOME/webapps/*
cp target/revenj.war $RESIN_HOME/webapps/
JAVA_EXE=$JAVA_HOME/bin/java resinctl console
