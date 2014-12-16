#!/bin/bash

mvn clean package
cd target
java -Ddatabase.host=${DBHOST} -jar spring.war &