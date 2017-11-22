#!/bin/bash

fw_depends mysql java maven

sed -i 's|localhost|'${DBHOST}'|g' minijax.properties

mvn clean package

java -XX:+UseNUMA -XX:+UseParallelGC -jar target/minijax-techempower-0.0.1.jar &
