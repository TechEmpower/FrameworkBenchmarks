#!/bin/bash

# load java environment variables
source $IROOT/java8.installed

mvn clean compile dependency:copy assembly:single

cd target
java -javaagent:quasar-core-0.7.2-jdk8.jar $PROPS -jar comsat-servlet-0.1-jar-with-dependencies.jar
