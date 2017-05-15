#!/bin/bash

fw_depends java maven

mvn clean compile assembly:single

cd target

java -server \
  -Xms1g -Xmx1g -XX:+UseNUMA -XX:+UseParallelGC -XX:+AggressiveOpts \
  -cp rapidoid-1.0-jar-with-dependencies.jar \
  lowlevel.Main profiles=production dbhost="$DBHOST" &
