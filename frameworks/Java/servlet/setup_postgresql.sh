#!/bin/bash

fw_depends postgresql

mvn clean compile war:war -P postgresql
rm -rf $RESIN_HOME/webapps/*
cp target/servlet.war $RESIN_HOME/webapps/
resinctl start
