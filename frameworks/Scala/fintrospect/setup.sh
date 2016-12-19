#!/bin/bash

fw_depends mysql java sbt

sbt clean

sbt 'oneJar' -batch

java -jar target/scala-2.11/*fintrospect*one-jar.jar &
