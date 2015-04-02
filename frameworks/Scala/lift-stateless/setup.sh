#!/bin/bash
export SBT_HOME=${IROOT}/sbt
export JAVA_HOME=/usr/lib/jvm/java-1.7.0-openjdk-amd64

sed -i 's|> ".*:3306|> "'"${DBHOST}"':3306|g' src/main/scala/Main.scala

${SBT_HOME}/bin/sbt update assembly

./run &