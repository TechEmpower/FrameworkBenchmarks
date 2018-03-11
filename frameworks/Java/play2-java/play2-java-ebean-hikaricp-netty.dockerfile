FROM tfb/sbt-java8:latest
ADD ./play2-java-ebean-hikaricp /play2-java-ebean-hikaricp
WORKDIR /play2-java-ebean-hikaricp
RUN sbt stage
CMD target/universal/stage/bin/play2-java-ebean-hikaricp \
    -Dplay.server.provider=play.core.server.NettyServerProvider
