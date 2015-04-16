#!/bin/bash

fw_depends java7 sbt

cd play2-java

rm -rf target/universal/stage/RUNNING_PID

sbt stage
target/universal/stage/bin/play2-java &
