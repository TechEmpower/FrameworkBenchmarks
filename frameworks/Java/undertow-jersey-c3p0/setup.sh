#!/bin/bash

fw_depends java maven

mvn clean package

java -jar target/undertow-jersey.jar -dbhost ${DBHOST} &
