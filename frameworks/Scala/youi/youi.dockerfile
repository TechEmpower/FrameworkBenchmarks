FROM sbtscala/scala-sbt:eclipse-temurin-21.0.6_7_1.10.11_2.13.16
WORKDIR /youi
COPY project project
COPY src src
COPY build.sbt build.sbt
RUN sbt pack
WORKDIR /youi/target/pack

EXPOSE 8080

CMD ["bin/main"]