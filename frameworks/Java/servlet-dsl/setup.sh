#!/bin/bash

# load java environment variables
source $IROOT/java7.installed

export RESIN_HOME=${IROOT}/resin-4.0.41

mvn clean compile war:war
rm -rf $RESIN_HOME/webapps/*
cp target/servlet-dsl.war $RESIN_HOME/webapps/
$RESIN_HOME/bin/resinctl start