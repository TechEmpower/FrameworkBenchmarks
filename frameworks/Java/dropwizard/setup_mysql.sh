#!/bin/bash

fw_depends mysql java maven

mvn -P mysql clean package

java -jar target/hello-world-0.0.1-SNAPSHOT.jar server hello-world-mysql.yml &
