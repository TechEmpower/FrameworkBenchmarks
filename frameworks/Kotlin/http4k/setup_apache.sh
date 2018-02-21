#!/bin/bash

fw_depends postgresql java

./gradlew clean build apache:uber

java -server -XX:+UseNUMA -XX:+UseParallelGC -XX:+AggressiveOpts -XX:+AlwaysPreTouch -jar apache/build/libs/http4k-apache-benchmark.jar &
