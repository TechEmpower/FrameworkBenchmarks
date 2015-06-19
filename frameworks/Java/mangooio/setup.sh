#!/bin/bash

sed -i 's|mongodb.host=.*|mongodb.host='"${DBHOST}"'|g' src/main/resources/application.conf

fw_depends java8 maven

mvn clean package -Dmaven.test.skip=true

java -jar target/mangooioapp.jar &
