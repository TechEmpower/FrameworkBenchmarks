#!/bin/bash
# load java environment variables
source $IROOT/java7.installed

sed -i 's|DATABASE_HOST|'"${DBHOST}"'|g' src/main/resources/hello/server.properties

mvn clean compile assembly:single
cd target
java -jar undertow-example-0.1-jar-with-dependencies.jar &