#!/bin/bash

# load java environment variables
source $IROOT/java8.installed

cd play2-java

# If application is running, clear old running app.
if [ -f ${TROOT}/play2-java/target/universal/stage/RUNNING_PID ]
then
  rm -f -r ${TROOT}/play2-java/target/universal/stage/RUNNING_PID
fi

${IROOT}/sbt/bin/sbt stage
target/universal/stage/bin/play2-java &
