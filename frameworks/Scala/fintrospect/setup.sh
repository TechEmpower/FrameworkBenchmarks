#!/bin/bash

fw_depends java sbt

sbt 'oneJar' -batch

java -Xms2G -Xmx2G -server -XX:+UseNUMA -XX:+UseParallelGC -XX:+AggressiveOpts -XX:+AlwaysPreTouch -jar target/scala-2.11/*fintrospect*one-jar.jar &
