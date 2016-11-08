#!/bin/bash

fw_depends java sbt

sbt 'oneJar' -batch

java -jar target/scala-2.11/http4s*one-jar.jar "${DBHOST}" &
