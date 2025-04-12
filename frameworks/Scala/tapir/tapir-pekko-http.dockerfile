FROM sbtscala/scala-sbt:eclipse-temurin-21.0.6_7_1.10.11_3.6.4

WORKDIR /tapir-benchmark
COPY pekko-http-server pekko-http-server
COPY common common
COPY project project
COPY build.sbt build.sbt
RUN sbt pekko-http-server/assembly

EXPOSE 8080
CMD ["java", "-Xms2G", "-Xmx2G", "-server", "-jar", "/tapir-benchmark/pekko-http-server/target/scala-3.6.4/tapir-pekko-http-server-assembly-1.0.0.jar"]