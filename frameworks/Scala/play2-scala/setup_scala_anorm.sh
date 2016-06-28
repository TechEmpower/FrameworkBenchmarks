#!/bin/bash

fw_depends java sbt

cd play2-scala-anorm
sed -i "s|jdbc:mysql:\/\/.*:3306|jdbc:mysql://${DBHOST}:3306|g" ${TROOT}/play2-scala-anorm/conf/application.conf

rm -f -r ${TROOT}/play2-scala-anorm/target/universal/stage/RUNNING_PID

# Stage application.
sbt stage

# Execute Start script in background.
${TROOT}/play2-scala-anorm/target/universal/stage/bin/play2-scala-anorm &
