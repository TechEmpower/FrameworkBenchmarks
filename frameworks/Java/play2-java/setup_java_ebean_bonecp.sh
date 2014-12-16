#!/bin/bash

cd play2-java-ebean-bonecp
${IROOT}/sbt/bin/sbt stage
target/universal/stage/bin/play2-java-ebean-bonecp &