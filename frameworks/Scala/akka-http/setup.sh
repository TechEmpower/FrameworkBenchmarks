#!/bin/bash

source $IROOT/java8.installed
export SBT_HOME=${IROOT}/sbt

${SBT_HOME}/bin/sbt 'assembly'

java -server -jar target/scala-2.11/akka-http-benchmark.jar