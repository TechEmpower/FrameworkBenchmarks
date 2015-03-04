#!/bin/bash

export JAVA_OPTS="-Xms2g -Xmx2g -XX:MaxPermSize=256m -XX:+UseG1GC -XX:MaxGCPauseMillis=25 -verbosegc -Xloggc:/tmp/wildfly_gc.log"

mvn clean initialize package -Pbenchmark -Ddatabase.host=${DBHOST}
target/wildfly-8.2.0.Final/bin/standalone.sh -b 0.0.0.0 &