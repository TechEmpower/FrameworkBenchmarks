#!/bin/bash

fw_depends mysql java resin maven

mvn clean package
rm -rf $RESIN_HOME/webapps/*
cp target/activeweb.war $RESIN_HOME/webapps/
resinctl start
