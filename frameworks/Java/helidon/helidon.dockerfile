FROM maven:3.6.1-jdk-11-slim as maven
WORKDIR /helidon
COPY src src
COPY pom.xml pom.xml
RUN mvn package -q

FROM openjdk:11.0.3-jdk-slim
WORKDIR /helidon
COPY --from=maven /helidon/target/libs libs
COPY --from=maven /helidon/target/benchmark.jar app.jar

EXPOSE 8080

CMD ["java", "-server", "-XX:-UseBiasedLocking", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-jar", "app.jar"]
