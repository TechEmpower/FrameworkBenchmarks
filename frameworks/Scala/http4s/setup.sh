#!/bin/bash

source $IROOT/java8.installed
export SBT_HOME=${IROOT}/sbt

#${SBT_HOME}/bin/sbt compile
${SBT_HOME}/bin/sbt 'oneJar'

java -jar target/scala-2.11/http4s*one-jar.jar
