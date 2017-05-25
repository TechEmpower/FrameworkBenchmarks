#!/bin/bash

fw_depends mysql java

./gradlew clean build jetty

java -server -XX:+UseNUMA -XX:+UseParallelGC -XX:+AggressiveOpts -XX:+AlwaysPreTouch -jar build/libs/http4k-standalone.jar &
