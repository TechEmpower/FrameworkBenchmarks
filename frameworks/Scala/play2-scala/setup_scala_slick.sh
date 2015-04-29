#!/bin/bash

fw_depends java7 sbt

cd play2-scala-slick
sed -i "s|jdbc:mysql:\/\/.*:3306|jdbc:mysql://${DBHOST}:3306|g" $TROOT/play2-scala-slick/conf/application.conf

rm -rf ${TROOT}/play2-scala-slick/target/universal/stage/RUNNING_PID

# Stage application.
sbt stage

# Execute Start script in background.
$TROOT/play2-scala-slick/target/universal/stage/bin/play2-scala-slick &
