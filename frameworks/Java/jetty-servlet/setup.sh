#!/bin/bash

fw_depends java maven

mvn clean compile assembly:single

cd target

java -server $JAVA_OPTS -jar jetty-servlet-example-0.2-jar-with-dependencies.jar &
