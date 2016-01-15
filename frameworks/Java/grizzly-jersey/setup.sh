#!/bin/bash

fw_depends java maven

mvn clean package

java -jar target/grizzly-jersey-example.jar -dbhost ${DBHOST} &
