#!/bin/bash

fw_depends java7 leiningen

cd hello
lein clean
lein uberjar
java -server -Xmx2g -XX:+UseG1GC -XX:MaxGCPauseMillis=10 -jar target/*-standalone.jar &
