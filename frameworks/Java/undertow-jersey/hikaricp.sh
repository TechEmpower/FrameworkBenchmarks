#!/bin/bash

fw_depends mysql java maven

mvn clean package -P hikaricp

java -jar target/undertow-jersey.jar -dbhost ${DBHOST} &
