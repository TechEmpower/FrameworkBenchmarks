FROM techempower/sbt:0.1

ADD ./play2-scala-anorm /play_app
WORKDIR /play_app

RUN sbt stage

CMD target/universal/stage/bin/play2-scala-anorm \
    -Dplay.server.provider=play.core.server.AkkaHttpServerProvider
