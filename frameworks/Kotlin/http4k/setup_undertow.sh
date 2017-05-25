#!/bin/bash

fw_depends mysql java

gradle wrapper

gradle clean build undertow
java -server -XX:+UseNUMA -XX:+UseParallelGC -XX:+AggressiveOpts -XX:+AlwaysPreTouch -jar build/libs/http4k-standalone.jar &
