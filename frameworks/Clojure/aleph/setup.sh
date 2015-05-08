#!/bin/bash

source $IROOT/java7.installed

source $IROOT/lein.installed

cd hello
lein clean
lein uberjar
java -server -Xmx2g -XX:+UseG1GC -XX:MaxGCPauseMillis=10 -jar target/*-standalone.jar &