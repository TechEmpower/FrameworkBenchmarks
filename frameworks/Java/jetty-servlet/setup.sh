#!/bin/bash

fw_depends java7 maven

mvn clean compile assembly:single

cd target
java -jar jetty-servlet-example-0.2-jar-with-dependencies.jar &
