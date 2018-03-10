FROM tfb/sbt-java8:latest
ADD ./play2-java-jooq-hikaricp /play2-java-jooq-hikaricp
WORKDIR /play2-java-jooq-hikaricp
RUN sbt stage
CMD target/universal/stage/bin/play2-java-jooq-hikaricp \
    -Dplay.server.provider=play.core.server.AkkaHttpServerProvider
