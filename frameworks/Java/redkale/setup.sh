#!/bin/bash

fw_depends java maven

mvn clean package

java -jar target/redkale-benchmark-0.0.1-SNAPSHOT.jar src/main/conf/config.json &
