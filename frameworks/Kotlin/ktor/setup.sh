#!/bin/bash

fw_depends java

./mvnw clean package -DskipTests=true
nohup java -jar target/tech-empower-framework-benchmark-1.0-SNAPSHOT.jar &

