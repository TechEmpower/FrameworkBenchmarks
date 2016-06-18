#!/bin/bash

fw_depends java8 scala sbt

sbt assembly -batch

java -jar target/scala-2.10/spray-benchmark-assembly-1.0.jar &
