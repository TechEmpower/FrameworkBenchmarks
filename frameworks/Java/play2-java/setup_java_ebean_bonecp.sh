#!/bin/bash

fw_depends java sbt

cd play2-java-ebean-bonecp

rm -rf target/universal/stage/RUNNING_PID

sbt stage
target/universal/stage/bin/play2-java-ebean-bonecp &
