#!/bin/bash

fw_depends java sbt

cd play2-java

rm -rf target/ project/target/ project/project/

sbt stage
target/universal/stage/bin/play2-java -Dplay.server.provider=play.core.server.AkkaHttpServerProvider &
