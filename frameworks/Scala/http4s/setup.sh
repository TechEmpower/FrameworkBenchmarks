#!/bin/bash

fw_depends postgresql java sbt

sbt assembly

java -jar target/scala-2.12/http4s-benchmark.jar "${DBHOST}" &
