#!/bin/bash

fw_depends java7 sbt

sed -i 's|127.0.0.1|'"${DBHOST}"'|g' src/main/resources/application.conf

sbt assembly && rm -rf target/scala-2.10/cache

java -jar target/scala-2.10/plain-benchmark-assembly-1.0.1.jar &
