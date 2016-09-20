#!/bin/bash

fw_depends java maven

sed -i 's|localhost|'"${DBHOST}"'|g' conf/application.conf

mvn clean package

cd target
java -server -XX:+UseNUMA -XX:+UseParallelGC -XX:+AggressiveOpts -jar jooby-1.0.jar &
