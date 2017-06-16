#!/bin/bash

fw_depends mysql java sbt

cd play2-java-ebean-hikaricp

rm -rf target/universal/stage/RUNNING_PID

sbt stage
target/universal/stage/bin/play2-java-ebean-hikaricp &
