#!/bin/bash

mvn clean compile assembly:single

cd target

java -jar servicetalk-jar-with-dependencies.jar
