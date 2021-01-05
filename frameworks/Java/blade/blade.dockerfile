FROM maven:3.6.1-jdk-11-slim as maven
WORKDIR /blade
COPY pom.xml pom.xml
COPY src src
RUN mvn package -q

FROM openjdk:11.0.3-jdk-slim
WORKDIR /blade
COPY --from=maven /blade/target/hello-blade-latest.jar app.jar

EXPOSE 9000

CMD ["java", "-server", "-Xms1G", "-Xmx1G", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+AggressiveOpts", "-jar", "app.jar","--server.performance=true"]
