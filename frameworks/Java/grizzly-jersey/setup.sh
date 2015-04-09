#!/bin/bash

# load java environment variables
source $IROOT/java7.installed

mvn clean package

java -jar target/grizzly-jersey-example.jar -dbhost ${DBHOST} &