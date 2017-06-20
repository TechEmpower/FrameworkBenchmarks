#!/bin/bash

fw_depends java maven

mvn -P servlet clean compile assembly:single

cd target
java -XX:+UseNUMA -XX:+UseParallelGC -jar jetty-servlet-example-0.2-jar-with-dependencies.jar &
