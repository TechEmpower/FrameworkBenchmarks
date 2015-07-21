#!/bin/bash

# load java environment variables
source $IROOT/java7.installed

mvn clean package
cd target
java -Ddatabase.host=${DBHOST} -jar spring.war &