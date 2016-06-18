#!/bin/bash

fw_depends java maven

mvn clean package 

cd target
java -Xms2G -Xmx2G -server -XX:+UseNUMA -XX:+UseParallelGC -XX:+AggressiveOpts -Dvertx.disableWebsockets=true -Dvertx.flashPolicyHandler=false -Dvertx.threadChecks=false -Dvertx.disableContextTimings=true -Dvertx.disableTCCL=true -jar vertx.benchmark-0.0.1-SNAPSHOT-fat.jar &
