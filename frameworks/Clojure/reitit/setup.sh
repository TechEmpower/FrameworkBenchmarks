#!/bin/bash

fw_depends java leiningen

cd hello
lein clean
lein uberjar
java -server -XX:+UseNUMA -XX:+UseParallelGC -XX:+AggressiveOpts -jar target/hello-reitit-standalone.jar
