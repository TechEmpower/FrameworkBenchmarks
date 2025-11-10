FROM litongjava/maven:3.8.8-jdk8u391 AS builder
WORKDIR /app

COPY pom.xml pom.xml
RUN mvn dependency:go-offline  -q

COPY src src
RUN mvn package -Passembly -q
RUN ls -l && ls -l target

FROM litongjava/jre:8u391-stable-slim

WORKDIR /app

COPY --from=builder /app/target/aio-socket-benchmark-1.0-jar-with-dependencies.jar /app/aio-socket-benchmark-1.0.jar

EXPOSE 8080

CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC","-cp", "/app/aio-socket-benchmark-1.0.jar","com.litongjava.aio.http.server.HttpServer"]