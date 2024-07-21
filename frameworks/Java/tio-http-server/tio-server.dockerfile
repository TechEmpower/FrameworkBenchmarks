FROM litongjava/maven:3.8.8-jdk8u391 AS builder
WORKDIR /app

COPY pom.xml pom.xml
RUN mvn dependency:go-offline

COPY src src
RUN mvn package -Passembly -q

FROM litongjava/jre:8u391-stable-slim

WORKDIR /app

COPY --from=builder /src/target/tio-http-server-benchmark-1.0.jar /app/target

EXPOSE 8080

CMD ["java", "-server", "-Xms1G", "-Xmx1G", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-Dpacket.handler.mode=queue1", "-jar", "/app/target/tio-http-server-benchmark-1.0.jar"]