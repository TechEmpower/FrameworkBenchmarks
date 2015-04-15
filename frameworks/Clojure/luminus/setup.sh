#!/bin/bash

fw_depends java7 resin leiningen 

sed -i 's|:subname "//.*:3306|:subname "//'"${DBHOST}"':3306|g' hello/src/hello/models/schema.clj

cd hello
lein clean
lein ring uberwar
rm -rf $RESIN_HOME/webapps/*
cp target/hello-luminus-standalone.war $RESIN_HOME/webapps/luminus.war
resinctl start
