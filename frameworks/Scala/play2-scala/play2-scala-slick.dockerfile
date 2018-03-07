FROM tfb/sbt:latest

COPY ./play2-scala-slick ./

RUN sbt stage

CMD target/universal/stage/bin/play2-scala-slick -Dplay.server.provider=play.core.server.AkkaHttpServerProvider
