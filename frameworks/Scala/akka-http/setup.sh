#!/bin/bash

fw_depends java8 sbt

sbt 'assembly'

java -server -jar target/scala-2.11/akka-http-benchmark.jar &
