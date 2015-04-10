#!/bin/bash
source $IROOT/java7.installed
export SBT_HOME=${IROOT}/sbt

sed -i 's|> ".*:3306|> "'"${DBHOST}"':3306|g' src/main/scala/Main.scala

${SBT_HOME}/bin/sbt update assembly

./run &