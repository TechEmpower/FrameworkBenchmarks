#!/bin/bash

fw_depends java maven

mvn clean compile assembly:single

cd target

java -jar servicetalk-jar-with-dependencies.jar
