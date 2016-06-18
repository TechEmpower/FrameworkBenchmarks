#!/bin/bash

<<<<<<< HEAD
fw_depends java8 scala sbt
=======
fw_depends java scala sbt
>>>>>>> upstream/master

sbt assembly -batch

java -jar target/scala-2.10/spray-benchmark-assembly-1.0.jar &
