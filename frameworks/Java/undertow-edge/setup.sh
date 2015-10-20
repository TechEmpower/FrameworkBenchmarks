#!/bin/bash

sed -i 's|127.0.0.1|'${DBHOST}'|g' src/main/resources/hello/server.properties

fw_depends java7 maven

mvn clean package
java -Djava.library.path=target/ -jar target/undertow-edge-0.1-jar-with-dependencies.jar &
