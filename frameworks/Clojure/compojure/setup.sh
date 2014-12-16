#!/bin/bash

# Path vars must be set here
export PATH="$JAVA_HOME/bin:$PATH"

sed -i 's|:subname "//.*:3306|:subname "//'"${DBHOST}"':3306|g' hello/src/hello/handler.clj

cd hello
$LEIN_HOME/bin/lein clean
$LEIN_HOME/bin/lein ring uberwar
rm -rf $RESIN_HOME/webapps/*
cp target/hello-compojure-standalone.war $RESIN_HOME/webapps/compojure.war
$RESIN_HOME/bin/resinctl start