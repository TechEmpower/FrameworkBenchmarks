#!/bin/bash

fw_depends mysql java maven

mvn clean package

java -jar target/grizzly-jersey-example.jar -dbhost ${DBHOST} &
