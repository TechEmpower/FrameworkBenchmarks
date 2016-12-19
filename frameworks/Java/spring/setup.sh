#!/bin/bash

fw_depends mysql java maven

mvn clean package
cd target
java -Ddatabase.host=${DBHOST} -jar spring.war &
