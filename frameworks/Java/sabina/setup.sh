#!/bin/bash

mvn clean package -Ddb.host=${DBHOST}
${JAVA_HOME}/bin/java -jar target/sabina-1.0.0-SNAPSHOT.jar &
