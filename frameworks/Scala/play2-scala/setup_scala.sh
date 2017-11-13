#!/bin/bash

fw_depends java sbt

cd play2-scala


rm -rf ${TROOT}/play2-scala/target/universal/stage/RUNNING_PID

# Stage application.
sbt stage

# Execute Start script in background.
${TROOT}/play2-scala/target/universal/stage/bin/play2-scala -J-server -J-Xms1g -J-Xmx1g -J-XX:NewSize=512m -J-XX:+UseG1GC -J-XX:MaxGCPauseMillis=30 -J-XX:-UseBiasedLocking -J-XX:+AlwaysPreTouch &
