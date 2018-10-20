FROM maven:3.5.3-jdk-8-slim as maven
WORKDIR /blade
COPY pom.xml pom.xml
COPY src src
RUN mvn clean package -q

FROM openjdk:8-jdk
WORKDIR /blade
COPY --from=maven /blade/target/hello-blade-latest.jar app.jar

CMD ["java", "-server", "-Xms1G", "-Xmx1G", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+AggressiveOpts", "-jar", "app.jar","--server.performance=true"]