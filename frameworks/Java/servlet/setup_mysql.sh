#!/bin/bash

fw_depends java resin maven mysql

mvn clean compile war:war -P mysql
rm -rf $RESIN_HOME/webapps/*
cp target/servlet.war $RESIN_HOME/webapps/
resinctl start
