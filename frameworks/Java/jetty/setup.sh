#!/bin/bash

# load java environment variables
source $IROOT/java8.installed
export JAVA_HOME=/usr/lib/jvm/java-8-oracle

mvn clean compile assembly:single

cd target
${JAVA_HOME}/bin/java -jar jetty-example-0.1-jar-with-dependencies.jar
