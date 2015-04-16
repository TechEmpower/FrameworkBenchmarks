#!/bin/bash

sed -i 's|mysql://.*:3306|mysql://'"${DBHOST}"':3306|g' src/main/resources/hello/server.properties
sed -i 's|postgresql://.*:5432|postgresql://'"${DBHOST}"':5432|g' src/main/resources/hello/server.properties

fw_depends java7 maven

mvn clean package
java -Djava.library.path=target/ -jar target/undertow-edge-0.1-jar-with-dependencies.jar &
