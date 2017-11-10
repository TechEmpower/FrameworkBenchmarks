#!/bin/bash

fw_depends postgresql java

./gradlew clean build jetty:uber

java -server -XX:+UseNUMA -XX:+UseParallelGC -XX:+AggressiveOpts -XX:+AlwaysPreTouch -jar jetty/build/libs/http4k-jetty-benchmark.jar &
