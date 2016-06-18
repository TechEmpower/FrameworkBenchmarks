#!/bin/bash

fw_depends java maven

mvn clean compile assembly:single

cd target
java -server -XX:+UseNUMA -XX:+UseParallelGC -XX:+AggressiveOpts -jar rapidoid-1.0-jar-with-dependencies.jar &
