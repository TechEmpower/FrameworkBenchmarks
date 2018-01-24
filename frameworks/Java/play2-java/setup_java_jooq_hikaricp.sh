#!/bin/bash

fw_depends mysql java sbt

cd play2-java-jooq-hikaricp

rm -rf target/ project/target/ project/project/

sbt stage
target/universal/stage/bin/play2-java-jooq-hikaricp -Dplay.server.provider=play.core.server.AkkaHttpServerProvider &
