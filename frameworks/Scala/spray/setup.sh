#!/bin/bash
export SBT_HOME=${IROOT}/sbt
export JAVA_HOME=/usr/lib/jvm/java-1.7.0-openjdk-amd64

${SBT_HOME}/bin/sbt assembly

java -jar target/scala-2.10/spray-benchmark-assembly-1.0.jar &