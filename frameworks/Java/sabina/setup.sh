#!/bin/bash

export JAVA_HOME=/opt/java8
mvn clean package -Ddb.host=${DBHOST}
${JAVA_HOME}/bin/java -jar target/sabina-1.0.0.jar &
