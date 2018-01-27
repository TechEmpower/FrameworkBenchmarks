#!/bin/bash

fw_depends postgresql java

./gradlew clean build sunhttp:uber

java -server -XX:+UseNUMA -XX:+UseParallelGC -XX:+AggressiveOpts -XX:+AlwaysPreTouch -jar netty/build/libs/http4k-sunhttp-benchmark.jar &
