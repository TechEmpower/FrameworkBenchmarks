#!/bin/bash

fw_depends mysql java maven

./mvnw clean package -DskipTests=true
nohup java -jar target/tech-empower-framework-benchmark-1.0-SNAPSHOT-netty-bundle.jar &

