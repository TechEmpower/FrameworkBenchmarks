#!/bin/bash

fw_depends java8 sbt

sbt 'oneJar'

java -jar target/scala-2.11/colossus*one-jar.jar
