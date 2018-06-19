FROM maven:3.5.3-jdk-10-slim as maven
WORKDIR /micronaut
COPY src src
COPY pom.xml pom.xml
RUN mvn package -q

FROM openjdk:10-jre-slim
WORKDIR /micronaut
COPY --from=maven /micronaut/target/hello-micronaut-0.1.jar app.jar
CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+AggressiveOpts", "-Dmicronaut.environments=benchmark", "-Dlog-root-level=OFF", "-jar", "app.jar"]