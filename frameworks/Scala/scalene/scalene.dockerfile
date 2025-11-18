FROM sbtscala/scala-sbt:eclipse-temurin-17.0.14_7_1.10.11_2.13.16

WORKDIR /scalene
COPY project project
COPY src src
COPY build.sbt build.sbt
RUN sbt assembly -batch

EXPOSE 8080

CMD ["java", "-server", "-Xmx2G", "-Xms2G", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-jar", "target/scala-2.13/scalene-benchmark-assembly-0.1.0-SNAPSHOT.jar"]
