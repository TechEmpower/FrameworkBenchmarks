#!/bin/bash
export SBT_HOME=${IROOT}/sbt
export JAVA_HOME=/usr/lib/jvm/java-1.7.0-openjdk-amd64

sed -i 's|127.0.0.1|'"${DBHOST}"'|g' src/main/resources/application.conf

${SBT_HOME}/bin/sbt assembly && rm -rf target/scala-2.10/cache

java -jar target/scala-2.10/plain-benchmark-assembly-1.0.1.jar &