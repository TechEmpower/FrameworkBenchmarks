#!/bin/bash

# load java environment variables
source $IROOT/java8.installed

gradle/wrapper clean war -Ddb.host=${DBHOST}

export RESIN_HOME=${IROOT}/resin-4.0.41
rm -rf $RESIN_HOME/webapps/*
cp build/libs/ROOT.war $RESIN_HOME/webapps
$RESIN_HOME/bin/resinctl start
