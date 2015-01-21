#!/bin/bash

sed -i "s|jdbc:mysql:\/\/.*:3306|jdbc:mysql://${DBHOST}:3306|g" play2-scala/conf/application.conf

cd play2-scala

# If application is running, clear old running app.
if [ -f ${TROOT}/play2-scala/target/universal/stage/RUNNING_PID ]
then
  rm -f -r ${TROOT}/play2-scala/target/universal/stage/RUNNING_PID
fi

# Stage application.
${IROOT}/sbt/bin/sbt stage

# Execute Start script in background.
${TROOT}/play2-scala/target/universal/stage/bin/play2-scala &