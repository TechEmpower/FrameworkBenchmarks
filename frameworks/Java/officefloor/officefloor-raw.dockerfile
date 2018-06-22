FROM maven:3.5.4-jdk-8 as maven
WORKDIR /officefloor
COPY src src
WORKDIR /officefloor/src
RUN mvn clean package

FROM openjdk:8
WORKDIR /officefloor
COPY --from=maven /officefloor/src/woof_raw/target/woof_raw-1.0.0.jar server.jar
CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+AggressiveOpts", "-jar", "server.jar"]
