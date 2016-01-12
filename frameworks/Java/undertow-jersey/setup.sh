#!/bin/bash

fw_depends java8 maven

mvn clean package

java -jar target/undertow-jersey.jar -dbhost ${DBHOST} &
