#!/bin/bash

fw_depends java maven

mvn clean package
cd target
java -Ddatabase.host=${DBHOST} -jar spring.war &
