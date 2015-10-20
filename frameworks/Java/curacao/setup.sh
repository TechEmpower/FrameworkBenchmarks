#!/bin/bash

fw_depends java7 sbt

sbt assembly

java -jar dist/curacao-standalone.jar &
