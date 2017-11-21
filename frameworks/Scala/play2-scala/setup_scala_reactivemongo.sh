#!/bin/bash

fw_depends mongodb java sbt

cd play2-scala-reactivemongo


rm -rf ${TROOT}/play2-scala-reactivemongo/target/universal/stage/RUNNING_PID

# Stage application.
sbt stage

# Execute Start script in background.
${TROOT}/play2-scala-reactivemongo/target/universal/stage/bin/play2-scala-reactivemongo &
