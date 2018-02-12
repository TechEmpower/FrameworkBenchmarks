#!/bin/bash

fw_depends mysql java8 resin-java8 maven

cd hello
mvn clean compile war:war
rm -rf $RESIN_HOME/webapps/*
cp target/tapestry.war $RESIN_HOME/webapps/tapestry.war
resinctl console
