#!/bin/bash

fw_depends java7 sbt

cd play2-scala-activate
sed -i 's|jdbc:mysql://.*:3306|jdbc:mysql://'"${DBHOST}"':3306|g' $TROOT/play2-scala-activate/conf/application.conf

rm -rf $TROOT/play2-scala-activate/target/universal/stage/RUNNING_PID

# Stage application.
sbt stage

# Execute Start script in background.
$TROOT/play2-scala-activate/target/universal/stage/bin/play2-scala-activate &
