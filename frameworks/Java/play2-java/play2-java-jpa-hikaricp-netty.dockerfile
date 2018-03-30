FROM tfb/sbt:latest
ADD ./play2-java-jpa-hikaricp /play2-java-jpa-hikaricp
WORKDIR /play2-java-jpa-hikaricp
RUN sbt stage
CMD target/universal/stage/bin/play2-java-jpa-hikaricp \
    -Dplay.server.provider=play.core.server.NettyServerProvider
