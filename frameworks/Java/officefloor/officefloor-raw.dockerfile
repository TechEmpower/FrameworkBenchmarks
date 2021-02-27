FROM maven:3.6.3 as maven
WORKDIR /officefloor
COPY src src
WORKDIR /officefloor/src/woof_benchmark_raw
RUN mvn -B clean package

FROM openjdk:15-slim
RUN apt-get update && apt-get install -y libjna-java
WORKDIR /officefloor
COPY --from=maven /officefloor/src/woof_benchmark_raw/target/woof_benchmark_raw-1.0.0.jar server.jar
EXPOSE 8080
CMD ["java", "-Xms2g", "-Xmx2g", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-jar", "server.jar"]
