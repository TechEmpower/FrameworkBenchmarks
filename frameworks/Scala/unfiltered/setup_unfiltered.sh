#!/bin/bash
source $IROOT/java7.installed

export SBT_HOME=${IROOT}/sbt

sed -i 's|jdbc:mysql://.*:3306|jdbc:mysql://'"${DBHOST}"':3306|g' src/main/resources/application.conf
sed -i 's|maxThreads = .*|maxThreads = '"${MAX_THREADS}"'|g' src/main/resources/application.conf

$SBT_HOME/bin/sbt assembly

cd target/scala-2.10
java -jar bench-assembly-1.0.0.jar &