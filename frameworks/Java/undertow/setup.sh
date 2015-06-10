#!/bin/bash
# load java environment variables
source $IROOT/java8.installed

sed -i 's|DATABASE_HOST|'"${DBHOST}"'|g' src/main/resources/hello/server.properties

mvn clean compile assembly:single
cd target
$JAVA_HOME/bin/java -jar undertow-example-0.1-jar-with-dependencies.jar &
