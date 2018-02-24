#!/bin/bash

fw_depends mysql java maven

cd grizzly-jersey
mvn clean package
java -jar target/grizzly-jersey-example.jar -dbhost ${DBHOST} &
