#!/bin/bash

cd play2-java
${IROOT}/sbt/bin/sbt stage
target/universal/stage/bin/play2-java &