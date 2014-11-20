#!/bin/bash

cd play2-java-jpa-bonecp
${IROOT}/sbt/bin/sbt stage
target/universal/stage/bin/play2-java-jpa-bonecp &