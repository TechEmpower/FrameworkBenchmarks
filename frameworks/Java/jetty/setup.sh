#!/bin/bash

fw_depends java maven

mvn clean compile assembly:single

cd target

java -server $JAVA_OPTS -jar jetty-example-0.1-jar-with-dependencies.jar
