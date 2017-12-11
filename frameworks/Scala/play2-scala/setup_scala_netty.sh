#!/bin/bash

fw_depends java sbt

cd play2-scala


rm -rf target/ project/target/ project/project/

# Stage application.
sbt stage

# Execute Start script in background.
${TROOT}/play2-scala/target/universal/stage/bin/play2-scala -Dplay.server.provider=play.core.server.NettyServerProvider -J-server -J-Xms1g -J-Xmx1g -J-XX:NewSize=512m -J-XX:+UseG1GC -J-XX:MaxGCPauseMillis=30 -J-XX:-UseBiasedLocking -J-XX:+AlwaysPreTouch &
