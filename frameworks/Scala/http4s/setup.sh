#!/bin/bash

fw_depends postgresql java sbt

sbt 'oneJar' -batch

java -jar target/scala-2.12/http4s*one-jar.jar "${DBHOST}" &
