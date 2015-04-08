#!/bin/bash

# load java environment variables
source $IROOT/java7.installed

export PLAY1_HOME=${IROOT}/play-1.2.5
export RESIN_HOME=${IROOT}/resin-4.0.41

sed -i 's|jdbc:mysql://.*:3306|jdbc:mysql://'"${DBHOST}"':3306|g' conf/application.conf

rm -rf $RESIN_HOME/webapps/*
$PLAY1_HOME/play war -o $RESIN_HOME/webapps/play1 --exclude benchmark_config.json
$RESIN_HOME/bin/resinctl start