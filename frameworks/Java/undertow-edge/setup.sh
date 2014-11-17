#!/bin/bash

sed -i 's|DATABASE_HOST|'"${DBHOST}"'|g' src/main/resources/hello/server.properties

mvn clean package
java -Djava.library.path=target/ -jar target/undertow-edge-0.1-jar-with-dependencies.jar &