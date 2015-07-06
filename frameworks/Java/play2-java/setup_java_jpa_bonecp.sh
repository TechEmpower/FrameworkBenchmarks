#!/bin/bash

fw_depends java8 sbt

sed -i 's|127.0.0.1|'${DBHOST}'|g' play2-java-jpa-bonecp/conf/application.conf

cd play2-java-jpa-bonecp

rm -rf target/universal/stage/RUNNING_PID

sbt stage
target/universal/stage/bin/play2-java-jpa-bonecp &
