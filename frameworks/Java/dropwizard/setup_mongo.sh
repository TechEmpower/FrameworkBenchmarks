#!/bin/bash

fw_depends mongodb java maven

mvn -P mongo clean package

java -jar target/hello-world-0.0.1-SNAPSHOT.jar server hello-world-mongo.yml &
