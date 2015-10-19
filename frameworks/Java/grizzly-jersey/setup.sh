#!/bin/bash

fw_depends java7 maven

mvn clean package

java -jar target/grizzly-jersey-example.jar -dbhost ${DBHOST} &
