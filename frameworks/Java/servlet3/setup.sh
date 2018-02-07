#!/bin/bash

fw_depends java resin maven

mvn clean compile war:war
rm -rf $RESIN_HOME/webapps/*
cp target/servlet3.war $RESIN_HOME/webapps
resinctl console
