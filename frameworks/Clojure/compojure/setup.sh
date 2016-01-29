#!/bin/bash

fw_depends java resin leiningen

sed -i 's|127.0.0.1|'"${DBHOST}"'|g' hello/src/hello/handler.clj

cd hello
lein clean
lein ring uberwar
rm -rf $RESIN_HOME/webapps/*
cp target/hello-compojure-standalone.war $RESIN_HOME/webapps/compojure.war
resinctl start
