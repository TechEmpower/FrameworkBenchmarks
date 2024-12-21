FROM litongjava/maven:3.8.8-jdk8u391 AS builder
WORKDIR /app

COPY pom.xml pom.xml
RUN mvn dependency:go-offline  -q

COPY src src
RUN mvn package -Passembly -q
RUN ls -l && ls -l target

FROM litongjava/jre:8u391-stable-slim

WORKDIR /app

COPY --from=builder /app/target/tio-http-server-benchmark-1.0.jar /app/target/tio-http-server-benchmark-1.0.jar

EXPOSE 8080

CMD ["java","-jar", "/app/target/tio-http-server-benchmark-1.0.jar"]