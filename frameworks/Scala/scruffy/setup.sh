#!/bin/bash
export SBT_HOME=${IROOT}/sbt

sed -i 's|database_host|'"${DBHOST}"'|g' src/main/scala/scruffy/examples/Test2Endpoint.scala

$SBT_HOME/bin/sbt assembly

java -jar target/scala-2.11/scruffy-benchmark-assembly-10.1.jar -Dhostname=${DBHOST} &