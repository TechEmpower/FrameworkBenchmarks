#!/bin/bash

fw_depends mysql java8 resin-java8 leiningen

cd hello
lein clean
lein ring uberwar
rm -rf $RESIN_HOME/webapps/*
cp target/hello-compojure-standalone.war $RESIN_HOME/webapps/compojure.war
resinctl console
