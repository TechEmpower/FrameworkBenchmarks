#!/bin/bash

source $IROOT/java7.installed

source $IROOT/lein.installed

lein clean

rm -rf target
# pack all dependencies into a single jar: target/pedestal-standalone.jar
lein uberjar
# -server is much faster
# 'lein run' passes '-client -XX:+TieredCompilation -XX:TieredStopAtLevel=1' which make it starts fast, but runs slow
java -server -jar target/pedestal-standalone.jar &