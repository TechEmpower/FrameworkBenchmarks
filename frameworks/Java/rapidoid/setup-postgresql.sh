#!/bin/bash

fw_depends postgresql java maven

mvn clean compile assembly:single

cd target

java -server \
  -Xms512m -Xmx2g -XX:+UseParallelGC -XX:+AggressiveOpts \
  -cp rapidoid-1.0-jar-with-dependencies.jar \
  highlevel.Main profiles=postgres,production dbhost="$DBHOST" &
