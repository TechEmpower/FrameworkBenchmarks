#!/bin/bash

# load java environment variables
source $IROOT/java7.installed

mvn clean compile assembly:single

cd target
java -jar jetty-servlet-example-0.1-jar-with-dependencies.jar &