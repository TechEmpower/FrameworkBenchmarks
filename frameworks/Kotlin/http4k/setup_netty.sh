#!/bin/bash

fw_depends postgresql java

./gradlew clean build netty:uber

java -server -XX:+UseNUMA -XX:+UseParallelGC -XX:+AggressiveOpts -XX:+AlwaysPreTouch -jar netty/build/libs/http4k-netty-benchmark.jar &
