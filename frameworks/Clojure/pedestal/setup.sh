#!/bin/bash

export JAVA_HOME=/usr/lib/jvm/java-1.7.0-openjdk-amd64

export LEIN_HOME=$IROOT/lein

$LEIN_HOME/bin/lein clean

rm -rf target
# pack all dependencies into a single jar: target/pedestal-standalone.jar
$LEIN_HOME/bin/lein uberjar
# -server is much faster
# 'lein run' passes '-client -XX:+TieredCompilation -XX:TieredStopAtLevel=1' which make it starts fast, but runs slow
$JAVA_HOME/bin/java -server -jar target/pedestal-standalone.jar &