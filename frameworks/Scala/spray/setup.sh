#!/bin/bash

fw_depends java scala sbt

sbt assembly -batch

java -jar target/scala-2.11/spray-benchmark-assembly-1.0.jar &
