#!/bin/bash

fw_depends mysql java sbt

cd play2-scala-anorm
sed -i "s|jdbc:mysql:\/\/.*:3306|jdbc:mysql://${DBHOST}:3306|g" ${TROOT}/play2-scala-anorm/conf/application.conf

rm -rf target/ project/target/ project/project/

# Stage application.
sbt stage

# Execute Start script in background.
${TROOT}/play2-scala-anorm/target/universal/stage/bin/play2-scala-anorm -Dplay.server.provider=play.core.server.AkkaHttpServerProvider &
