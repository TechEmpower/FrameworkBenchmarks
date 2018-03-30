FROM tfb/sbt:latest

ADD ./play2-scala-reactivemongo /play_app
WORKDIR /play_app

RUN sbt stage

CMD target/universal/stage/bin/play2-scala-reactivemongo \
    -Dplay.server.provider=play.core.server.NettyServerProvider
