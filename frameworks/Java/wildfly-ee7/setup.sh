#!/bin/bash

fw_depends mysql java maven

export JAVA_OPTS="-Djava.net.preferIPv4Stack=true -Xms2g -Xmx2g -XX:+UseG1GC -XX:MaxGCPauseMillis=50"

mvn clean initialize package -Pbenchmark -Ddatabase.host=${DBHOST}
target/wildfly-11.0.0.Final/bin/standalone.sh -b 0.0.0.0 &
