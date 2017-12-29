#!/bin/bash

fw_depends postgresql java

./gradlew clean build undertow:uber

java -server -XX:+UseNUMA -XX:+UseParallelGC -XX:+AggressiveOpts -XX:+AlwaysPreTouch -jar undertow/build/libs/http4k-undertow-benchmark.jar &
