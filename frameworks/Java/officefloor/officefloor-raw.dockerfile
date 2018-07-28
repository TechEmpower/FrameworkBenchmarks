FROM maven:3.5.4-jdk-10 as maven
WORKDIR /officefloor
COPY src src
WORKDIR /officefloor/src/woof_raw
RUN mvn -q clean package

FROM openjdk:10
WORKDIR /officefloor
COPY --from=maven /officefloor/src/woof_raw/target/woof_raw-1.0.0.jar server.jar
CMD ["java", "-Xms2g", "-Xmx2g", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+AggressiveOpts", "-jar", "server.jar"]
