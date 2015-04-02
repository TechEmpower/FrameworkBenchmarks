#!/bin/bash
export SBT_HOME=${IROOT}/sbt
export JAVA_HOME=/usr/lib/jvm/java-1.7.0-openjdk-amd64

${SBT_HOME}/bin/sbt update compile

${SBT_HOME}/bin/sbt -Ddb.host=${DBHOST} run &