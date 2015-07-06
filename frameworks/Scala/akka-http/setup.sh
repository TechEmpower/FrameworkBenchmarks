#!/bin/bash

fw_depends java8 sbt

sed -i 's|dbhost: "0.0.0.0"|dbhost: "'${DBHOST}'"|g' src/main/resources/application.conf
sed -i 's|0.0.0.0:3306|'${DBHOST}':3306|g' src/main/resources/application.conf

sbt 'assembly'

java -server -jar target/scala-2.11/akka-http-benchmark.jar &
