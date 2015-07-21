#!/bin/bash

# load java environment variables
source $IROOT/java8.installed

mvn clean package -DskipTests -Ddb.host=${DBHOST}

export RESIN_HOME=${IROOT}/resin-4.0.41
rm -rf $RESIN_HOME/webapps/*
cp target/sabina-1.0.0.war $RESIN_HOME/webapps
$RESIN_HOME/bin/resinctl start
