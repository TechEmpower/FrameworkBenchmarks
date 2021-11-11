FROM maven:3.6.1-jdk-11-slim as maven

WORKDIR /activej
COPY pom.xml pom.xml
COPY src src
RUN mvn compile assembly:single -q

FROM openjdk:11.0.3-jdk-slim
WORKDIR /activej
COPY --from=maven /activej/target/activej-server-benchmark-0.0.1-SNAPSHOT-jar-with-dependencies.jar app.jar
CMD ["java", "-Xms2G", "-Xmx2G", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+AggressiveOpts", "-DHttpServerConnection.initialWriteBufferSize=4096", "-DHttpHeadersMultimap.initialSize=16", "-jar", "app.jar"]