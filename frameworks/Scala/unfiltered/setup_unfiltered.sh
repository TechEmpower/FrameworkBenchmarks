#!/bin/bash

fw_depends jmysql ava scala sbt

sed -ie 's|jdbc:mysql://.*:3306|jdbc:mysql://'"${DBHOST}"':3306|g' src/main/resources/application.conf
sed -ie 's|maxThreads = .*|maxThreads = '"${MAX_THREADS}"'|g' src/main/resources/application.conf

sbt assembly -batch

cd target/scala-2.11
java -jar bench-assembly-1.0.0.jar &
