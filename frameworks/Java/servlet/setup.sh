#!/bin/bash

fw_depends java resin maven

mvn clean install
rm -rf $RESIN_HOME/webapps/*
cp target/servlet.war $RESIN_HOME/webapps/
resinctl start
