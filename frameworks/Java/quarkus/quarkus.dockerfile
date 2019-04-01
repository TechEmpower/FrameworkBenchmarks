FROM maven:3.6-jdk-11-slim as maven
WORKDIR /quarkus
COPY pom.xml pom.xml
RUN mvn dependency:go-offline -q
COPY src src
RUN mvn package -q

FROM openjdk:11-jre-slim
WORKDIR /quarkus
COPY --from=maven /quarkus/target/lib lib
COPY --from=maven /quarkus/target/benchmark-1.0-SNAPSHOT-runner.jar app.jar
CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-jar", "app.jar"]
