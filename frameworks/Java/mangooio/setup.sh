#!/bin/bash

# load java environment variables
source $IROOT/java8.installed

sed -i 's|mongodb.host=.*|mongodb.host='"${DBHOST}"'|g' src/main/resources/application.conf

mvn clean package -Dmaven.test.skip=true

java -jar target/mangooioapp.jar &