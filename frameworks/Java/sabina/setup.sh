#!/bin/bash

export JAVA_HOME=/opt/java8
mvn clean package -DskipTests -Ddb.host=${DBHOST}
${JAVA_HOME}/bin/java -jar target/sabina-1.0.0.jar &
