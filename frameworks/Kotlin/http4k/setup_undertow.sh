#!/bin/bash

fw_depends postgresql java

./gradlew clean build undertow

java -server -XX:+UseNUMA -XX:+UseParallelGC -XX:+AggressiveOpts -XX:+AlwaysPreTouch -jar build/libs/http4k-standalone.jar &
