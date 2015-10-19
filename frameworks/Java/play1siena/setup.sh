#!/bin/bash

sed -i 's|jdbc:mysql://.*:3306|jdbc:mysql://'"${DBHOST}"':3306|g' conf/application.conf

fw_depends java7 play1 siena resin

rm -rf $RESIN_HOME/webapps/*
play war -o $RESIN_HOME/webapps/play1 --exclude benchmark_config.json
resinctl start
