#!/bin/bash

fw_depends mysql java maven

./mvnw clean package
nohup java -jar target/tech-empower-framework-benchmark-1.0-SNAPSHOT-cio-bundle.jar &

