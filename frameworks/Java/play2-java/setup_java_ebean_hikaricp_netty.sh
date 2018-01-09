#!/bin/bash

fw_depends mysql java sbt

cd play2-java-ebean-hikaricp

rm -rf target/ project/target/ project/project/

sbt stage
target/universal/stage/bin/play2-java-ebean-hikaricp -Dplay.server.provider=play.core.server.NettyServerProvider &
