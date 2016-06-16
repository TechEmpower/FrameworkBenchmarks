#!/bin/bash

# specify build requirements
fw_depends java maven

# build project
mvn clean compile assembly:single

# run server
cd target
java -server -Xss256k -XX:+UseNUMA -XX:+UseParallelGC -XX:+AggressiveOpts -jar jlhttp-1.0-jar-with-dependencies.jar &
