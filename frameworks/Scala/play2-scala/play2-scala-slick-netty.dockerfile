FROM techempower/sbt:0.1

COPY ./play2-scala-slick ./

RUN sbt stage

CMD target/universal/stage/bin/play2-scala-slick -Dplay.server.provider=play.core.server.NettyServerProvider
