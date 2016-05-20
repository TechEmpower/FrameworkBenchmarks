#!/bin/bash

fw_depends java sbt

sbt clean assembly -batch

java -jar target/scala-2.11/finatra-benchmark.jar -log.level=ERROR
