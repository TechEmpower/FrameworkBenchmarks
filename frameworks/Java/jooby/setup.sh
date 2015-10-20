#!/bin/bash

fw_depends java8 maven

mvn clean package

cd target
java -server -XX:+UseNUMA -XX:+UseParallelGC -XX:+AggressiveOpts -jar jooby-1.0.jar &
