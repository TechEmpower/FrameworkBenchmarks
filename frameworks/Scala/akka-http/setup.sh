#!/bin/bash

fw_depends mysql java sbt

sbt 'assembly' -batch

java -server -jar target/scala-2.11/akka-http-benchmark.jar &
