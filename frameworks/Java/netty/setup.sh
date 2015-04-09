#!/bin/bash

# load java environment variables
source $IROOT/java7.installed

mvn clean compile assembly:single

cd target
java -server -XX:+UseNUMA -XX:+UseParallelGC -XX:+AggressiveOpts -jar netty-example-0.1-jar-with-dependencies.jar &