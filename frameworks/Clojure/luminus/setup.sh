#!/bin/bash
export JAVA_HOME=/usr/lib/jvm/java-1.7.0-openjdk-amd64
export RESIN_HOME=${IROOT}/resin-4.0.41

# Path vars must be set here
export PATH="$JAVA_HOME/bin:$PATH"

sed -i 's|:subname "//.*:3306|:subname "//'"${DBHOST}"':3306|g' hello/src/hello/models/schema.clj

cd hello
$IROOT/lein/bin/lein clean
$IROOT/lein/bin/lein ring uberwar
rm -rf $RESIN_HOME/webapps/*
cp target/hello-luminus-standalone.war $RESIN_HOME/webapps/luminus.war
$RESIN_HOME/bin/resinctl start