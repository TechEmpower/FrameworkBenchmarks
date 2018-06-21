FROM maven:3.5.3-jdk-10-slim as maven
WORKDIR /officefloor
COPY src src
WORKDIR /officefloor/src
RUN mvn clean package

FROM openjdk:10-jre-slim
WORKDIR /officefloor
COPY --from=maven /officefloor/src/benchmarks/woof_benchmark/target/officefloor-1.0.0-jar-with-dependencies.jar app.jar
CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+AggressiveOpts", "-cp", "app.jar", "net.officefloor.OfficeFloorMain"]
