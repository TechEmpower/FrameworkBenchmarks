FROM sbtscala/scala-sbt:eclipse-temurin-21.0.6_7_1.10.11_3.6.4

WORKDIR /tapir-benchmark
COPY http4s-server-zio http4s-server-zio
COPY common common
COPY project project
COPY build.sbt build.sbt
RUN sbt http4s-server-zio/assembly

EXPOSE 8080
CMD ["java", "-Xms2G", "-Xmx2G", "-server", "-jar", "/tapir-benchmark/http4s-server-zio/target/scala-3.6.4/tapir-http4s-server-zio-assembly-1.0.0.jar"]