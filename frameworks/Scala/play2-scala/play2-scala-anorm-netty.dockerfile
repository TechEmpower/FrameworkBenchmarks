FROM tfb/sbt:latest

COPY ./play2-scala-anorm ./

RUN sbt stage

CMD target/universal/stage/bin/play2-scala-anorm -Dplay.server.provider=play.core.server.NettyServerProvider
