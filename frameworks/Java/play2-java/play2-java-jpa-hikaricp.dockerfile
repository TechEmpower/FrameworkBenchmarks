FROM hseeberger/scala-sbt:8u151-2.12.5-1.1.2
WORKDIR /play2
COPY play2-java-jpa-hikaricp .
RUN sbt stage
CMD ["target/universal/stage/bin/play2-java-jpa-hikaricp", "-Dplay.server.provider=play.core.server.AkkaHttpServerProvider"]
