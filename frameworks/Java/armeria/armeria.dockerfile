FROM maven:3.5.3-jdk-10-slim as maven
WORKDIR /armeria
COPY src src
COPY pom.xml pom.xml
RUN mvn compile assembly:single -q

FROM openjdk:10-jre-slim
WORKDIR /armeria
COPY --from=maven /armeria/target/hello-1.0-SNAPSHOT-jar-with-dependencies.jar app.jar
CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+AggressiveOpts", "-jar", "app.jar"]