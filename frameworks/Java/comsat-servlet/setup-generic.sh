#!/bin/bash

# load java environment variables
source $IROOT/java8.installed

mvn clean compile dependency:copy assembly:single

cd target
QUASAR_AGENT=`ls quasar-core-*.jar`
FATJAR=`ls comsat-servlet-*.jar`
java -javaagent:$QUASAR_AGENT $PROPS -jar $FATJAR
