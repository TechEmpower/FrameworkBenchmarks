#!/bin/bash

fw_depends java sbt

sed -i 's|127.0.0.1:3306|'${DBHOST}':3306|g' play2-java-ebean-bonecp/conf/application.conf

cd play2-java-ebean-bonecp

rm -rf target/universal/stage/RUNNING_PID

sbt stage
target/universal/stage/bin/play2-java-ebean-bonecp &
