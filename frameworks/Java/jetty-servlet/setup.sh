#!/bin/bash

mvn clean compile assembly:single

cd target
java -jar jetty-servlet-example-0.1-jar-with-dependencies.jar &