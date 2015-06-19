#!/bin/bash

fw_depends java8 maven

mvn clean compile assembly:single

cd target
java -jar jetty-example-0.1-jar-with-dependencies.jar
