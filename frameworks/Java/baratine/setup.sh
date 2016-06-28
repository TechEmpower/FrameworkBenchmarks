#!/bin/bash

fw_depends java maven

mvn clean package

java -jar target/testTechempowerBaratine-0.0.1-SNAPSHOT.jar ${DBHOST}
