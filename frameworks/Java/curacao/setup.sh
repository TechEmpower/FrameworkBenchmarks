#!/bin/bash

fw_depends java resin maven

mvn clean compile war:war
rm -rf $RESIN_HOME/webapps/*
cp target/curacao.war $RESIN_HOME/webapps
$RESIN_HOME/bin/resinctl start
