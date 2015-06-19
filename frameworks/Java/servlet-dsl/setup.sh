#!/bin/bash

fw_depends java7 resin maven

mvn clean compile war:war
rm -rf $RESIN_HOME/webapps/*
cp target/servlet-dsl.war $RESIN_HOME/webapps/
resinctl start
