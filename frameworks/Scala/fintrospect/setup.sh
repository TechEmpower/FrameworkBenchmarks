#!/bin/bash

fw_depends mysql java sbt

sbt clean

sbt 'oneJar' -batch

java -Dcom.twitter.finagle.toggle.flag.overrides=com.twitter.http.UseNetty4=1.0 -server -XX:+UseNUMA -XX:+UseParallelGC -XX:+AggressiveOpts -XX:+AlwaysPreTouch -jar target/scala-2.12/*fintrospect*one-jar.jar &
