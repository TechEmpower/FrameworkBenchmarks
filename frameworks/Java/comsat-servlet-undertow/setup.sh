#!/bin/bash

# load java environment variables
source $IROOT/java7.installed

mvn clean compile dependency:copy assembly:single

cd target
java -javaagent:quasar-core-0.7.2.jar -jar comsat-servlet-undertow-0.1-jar-with-dependencies.jar
