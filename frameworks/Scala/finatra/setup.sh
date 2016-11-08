#!/bin/bash

fw_depends java sbt

sbt clean assembly -batch

java -Dcom.twitter.util.events.sinkEnabled=false -Xmx2000M -Xms2000M -Xmn1750M -XX:MetaspaceSize=128M -XX:ParallelGCThreads=4 -XX:+CMSScavengeBeforeRemark -XX:TargetSurvivorRatio=90 -XX:+UseConcMarkSweepGC -XX:CMSInitiatingOccupancyFraction=70 -XX:+UseCMSInitiatingOccupancyOnly -XX:TargetSurvivorRatio=60 -jar target/scala-2.11/finatra-benchmark.jar -log.level=ERROR -http.response.charset.enabled=false
