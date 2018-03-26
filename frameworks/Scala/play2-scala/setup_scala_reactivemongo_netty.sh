#!/bin/bash

fw_depends mongodb java sbt

cd play2-scala-reactivemongo


rm -rf target/ project/target/ project/project/

# Stage application.
sbt stage

# Execute Start script in background.
${TROOT}/play2-scala-reactivemongo/target/universal/stage/bin/play2-scala-reactivemongo -Dplay.server.provider=play.core.server.NettyServerProvider &
