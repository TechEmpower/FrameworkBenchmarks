#!/bin/bash

# load java environment variables
source $IROOT/java8.installed

mvn clean compile assembly:single

cd target
java -jar jetty-example-0.1-jar-with-dependencies.jar &
