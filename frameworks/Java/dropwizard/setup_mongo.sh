#!/bin/bash

fw_depends java maven

sed -i 's|host: 127.0.0.1|host: '"${DBHOST}"'|g' hello-world-mongo.yml

mvn -P mongo clean package

java -jar target/hello-world-0.0.1-SNAPSHOT.jar server hello-world-mongo.yml &
