FROM tfb/sbt:latest
ADD ./play2-java /play2-java
WORKDIR /play2-java
RUN sbt stage
CMD target/universal/stage/bin/play2-java \
    -Dplay.server.provider=play.core.server.NettyServerProvider
