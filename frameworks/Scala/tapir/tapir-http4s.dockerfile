FROM sbtscala/scala-sbt:eclipse-temurin-21.0.6_7_1.10.11_3.6.4

WORKDIR /tapir-benchmark
COPY http4s-server http4s-server
COPY common common
COPY project project
COPY build.sbt build.sbt
RUN sbt http4s-server/assembly

EXPOSE 8080
CMD ["java", "-Xms2G", "-Xmx2G", "-server", "-jar", "/tapir-benchmark/http4s-server/target/scala-3.6.4/tapir-http4s-server-assembly-1.0.0.jar"]