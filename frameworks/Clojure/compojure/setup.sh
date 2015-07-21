#!/bin/bash

source $IROOT/java7.installed

source $IROOT/lein.installed

export RESIN_HOME=${IROOT}/resin-4.0.41

sed -i 's|:subname "//.*:3306|:subname "//'"${DBHOST}"':3306|g' hello/src/hello/handler.clj

cd hello
lein clean
lein ring uberwar
rm -rf $RESIN_HOME/webapps/*
cp target/hello-compojure-standalone.war $RESIN_HOME/webapps/compojure.war

$RESIN_HOME/bin/resinctl start

# preform an initial request to warm up the server
# initial requests were taking >15 seconds, causing fails in the tests
echo "Sleeping, then executing an initial request to ensure server is in a responsive state"
sleep 30
curl -m 60 http://localhost:8080/compojure/ > /dev/null 2>&1
