FROM maven:3.6.1-jdk-11-slim as maven
WORKDIR /officefloor
COPY src src
WORKDIR /officefloor/src/woof_benchmark_raw
RUN mvn -q clean package

FROM openjdk:11.0.3-jdk-slim
RUN apt-get update && apt-get install -y libjna-java
WORKDIR /officefloor
COPY --from=maven /officefloor/src/woof_benchmark_raw/target/woof_benchmark_raw-1.0.0.jar server.jar
CMD ["java", "-Xms2g", "-Xmx2g", "-server", "-XX:+UseNUMA", "-jar", "server.jar"]
