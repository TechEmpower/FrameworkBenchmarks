FROM maven:3.5.3-jdk-8-slim as maven
WORKDIR /helidon
COPY src src
COPY pom.xml pom.xml
RUN mvn package -q

FROM openjdk:8-jre-slim
WORKDIR /helidon
COPY --from=maven /helidon/target/libs libs
COPY --from=maven /helidon/target/benchmark.jar app.jar
CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-Dlogging.level.root=DEBUG", "-jar", "app.jar"]
