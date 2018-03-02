#!/bin/bash

fw_depends java tomcat maven

cd servlet3
mvn clean compile war:war
rm -rf $CATALINA_HOME/webapps/*
cp target/servlet3.war $CATALINA_HOME/webapps
$CATALINA_HOME/bin/startup.sh
