#!/bin/bash
export SBT_HOME=${IROOT}/sbt
source $IROOT/java7.installed

${SBT_HOME}/bin/sbt assembly

java -jar target/scala-2.10/spray-benchmark-assembly-1.0.jar &