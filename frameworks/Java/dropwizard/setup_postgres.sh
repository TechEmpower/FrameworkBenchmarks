#!/bin/bash

fw_depends java maven

sed -i 's|url: jdbc:postgresql://.*/hello_world|url: jdbc:postgresql://'"${DBHOST}"':5432/hello_world|g' hello-world-postgres.yml

mvn -P postgres clean package

java -jar target/hello-world-0.0.1-SNAPSHOT.jar server hello-world-postgres.yml &
