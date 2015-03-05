#!/bin/bash

sed -i 's|jdbc:mysql://.*:3306|jdbc:mysql://'"${DBHOST}"':3306|g' conf/application.conf

rm -rf $RESIN_HOME/webapps/*
$PLAY1_HOME/play war -o $RESIN_HOME/webapps/play1 --exclude benchmark_config.json
$RESIN_HOME/bin/resinctl start