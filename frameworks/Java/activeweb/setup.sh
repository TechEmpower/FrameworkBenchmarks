#!/bin/bash

fw_depends mysql java resin maven

# The tests are broken on Java 9.
mvn clean package -DskipTests

rm -rf $RESIN_HOME/webapps/*
cp target/activeweb.war $RESIN_HOME/webapps/
resinctl start
