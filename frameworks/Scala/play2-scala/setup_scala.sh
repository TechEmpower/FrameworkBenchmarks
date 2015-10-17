#!/bin/bash

fw_depends java7 sbt

cd play2-scala
sed -i "s|jdbc:mysql:\/\/.*:3306|jdbc:mysql://${DBHOST}:3306|g" $TROOT/play2-scala/conf/application.conf

# Clear old running app.
rm -rf $TROOT/play2-scala/target/universal/stage/RUNNING_PID

# Stage application.
sbt stage

# Execute Start script in background.
$TROOT/play2-scala/target/universal/stage/bin/play2-scala &
