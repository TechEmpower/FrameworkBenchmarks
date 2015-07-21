#!/bin/bash
# load java environment variables
source $IROOT/java7.installed

sed -i 's|mysql://.*:3306|mysql://'"${DBHOST}"':3306|g' src/main/resources/hello/server.properties
sed -i 's|postgresql://.*:5432|postgresql://'"${DBHOST}"':5432|g' src/main/resources/hello/server.properties

mvn clean package
java -Djava.library.path=target/ -jar target/undertow-edge-0.1-jar-with-dependencies.jar &