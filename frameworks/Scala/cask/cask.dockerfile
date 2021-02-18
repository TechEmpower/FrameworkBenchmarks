FROM hseeberger/scala-sbt:8u151-2.12.5-1.1.2
WORKDIR /cask
COPY project project
COPY src src
COPY build.sbt build.sbt
RUN sbt assembly -batch

EXPOSE 8080

CMD ["java", "-server", "-Xms1g", "-Xmx1g", "-XX:NewSize=512m", "-XX:MaxNewSize=512m", "-XX:+UseParallelGC", "-XX:+AggressiveOpts", "-XX:+UseNUMA", "-XX:-UseBiasedLocking", "-XX:+AlwaysPreTouch", "-jar", "target/scala-2.12/cask-example-assembly-1.0.jar"]
