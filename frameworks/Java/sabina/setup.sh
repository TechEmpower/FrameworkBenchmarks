#!/bin/bash

# load java environment variables
source $IROOT/java8.installed

mvn clean package -DskipTests -Ddb.host=${DBHOST}
${JAVA_HOME}/bin/java -jar target/sabina-1.0.0.jar &
