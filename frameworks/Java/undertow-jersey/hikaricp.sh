#!/bin/bash

fw_depends mysql java maven

mvn clean package -p hikaricp

java -jar target/undertow-jersey.jar -dbhost ${DBHOST} &
