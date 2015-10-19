#!/bin/bash

fw_depends java7 maven

export JAVA_OPTS="-Xms2g -Xmx2g -XX:MaxPermSize=256m -XX:+UseG1GC -XX:MaxGCPauseMillis=25 -verbosegc -Xloggc:/tmp/wildfly_gc.log"

mvn clean initialize package -Pbenchmark -Ddatabase.host=${DBHOST}
target/wildfly-9.0.0.Beta2/bin/standalone.sh -b 0.0.0.0 &
