#!/bin/bash

fw_depends java maven

mvn clean compile assembly:single

cd target

java -server $JAVA_OPTS_TFB -jar jetty-servlet-example-0.2-jar-with-dependencies.jar &
