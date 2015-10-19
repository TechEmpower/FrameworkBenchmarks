#!/bin/bash

sed -i 's|url: jdbc:mysql://.*/hello_world|url: jdbc:mysql://'"${DBHOST}"':3306/hello_world|g' hello-world-mysql.yml

fw_depends java7 maven

mvn -P mysql clean package

java -jar target/hello-world-0.0.1-SNAPSHOT.jar server hello-world-mysql.yml &
