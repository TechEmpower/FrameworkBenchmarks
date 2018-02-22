#!/bin/bash

fw_depends java tomcat maven

mvn clean compile war:war -P sync
rm -rf $CATALINA_HOME/webapps/*
cp target/servlet3.war $CATALINA_HOME/webapps
$CATALINA_HOME/bin/startup.sh
