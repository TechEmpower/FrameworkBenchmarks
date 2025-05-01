FROM sbtscala/scala-sbt:eclipse-temurin-21.0.6_7_1.10.11_2.12.20
WORKDIR /cask
COPY project project
COPY src src
COPY build.sbt build.sbt
RUN sbt assembly -batch

EXPOSE 8080

CMD ["java", "-server", "-Xms1g", "-Xmx1g", "-XX:NewSize=512m", "-XX:MaxNewSize=512m", "-XX:+UseParallelGC", "-XX:+UseNUMA", "-XX:+AlwaysPreTouch", "-jar", "target/scala-2.12/cask-example-assembly-1.0.jar"]
