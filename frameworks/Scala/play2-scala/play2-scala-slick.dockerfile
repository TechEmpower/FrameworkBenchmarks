FROM tfb/sbt:latest

ADD ./play2-scala-slick /play_app
WORKDIR /play_app

RUN sbt stage

CMD target/universal/stage/bin/play2-scala-slick \
    -Dplay.server.provider=play.core.server.AkkaHttpServerProvider
