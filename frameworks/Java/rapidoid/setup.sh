#!/bin/bash

# load java environment variables
source $IROOT/java7.installed

mvn clean compile assembly:single

cd target
java -server -XX:+UseNUMA -XX:+UseParallelGC -XX:+AggressiveOpts -jar rapidoid-1.0-jar-with-dependencies.jar &
