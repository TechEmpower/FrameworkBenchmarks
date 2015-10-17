#!/bin/bash

fw_depends java7 sbt

sed -i 's|127.0.0.1|'${DBHOST}'|g' src/main/scala/scruffy/examples/Test2Endpoint.scala

sbt assembly

java -jar target/scala-2.11/scruffy-benchmark-assembly-11.0.jar -Dhostname=${DBHOST} &
