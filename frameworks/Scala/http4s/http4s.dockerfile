FROM hseeberger/scala-sbt:8u151-2.12.5-1.1.2
WORKDIR /http4s
COPY project project
COPY src src
COPY build.sbt build.sbt
RUN sbt assembly -batch
CMD ["java", "-jar", "target/scala-2.12/http4s-assembly-1.0.jar", "tfb-database"]
