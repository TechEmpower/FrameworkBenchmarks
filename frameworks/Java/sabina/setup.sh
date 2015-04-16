#!/bin/bash

fw_depends java8 maven

mvn clean package -DskipTests -Ddb.host=${DBHOST}
$JAVA_HOME/bin/java -jar target/sabina-1.0.0.jar &
