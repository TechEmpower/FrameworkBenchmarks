#!/bin/bash

fw_depends java sbt

sbt assembly

java -jar dist/curacao-standalone.jar &
