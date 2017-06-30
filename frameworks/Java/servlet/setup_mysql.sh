#!/bin/bash

fw_depends mysql

mvn clean install -P mysql
rm -rf $RESIN_HOME/webapps/*
cp target/servlet.war $RESIN_HOME/webapps/
resinctl start
