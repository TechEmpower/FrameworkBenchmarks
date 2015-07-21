#!/bin/bash

# load java environment variables
source $IROOT/java7.installed

mvn clean package

java -jar target/dropwizard-mongodb-0.0.1-SNAPSHOT.jar server hello-world.yml &