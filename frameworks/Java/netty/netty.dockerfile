FROM maven:3.5.3-jdk-10-slim as maven
WORKDIR /netty
COPY pom.xml pom.xml
COPY src src
RUN mvn compile assembly:single -q

FROM openjdk:10-jre-slim
WORKDIR /netty
COPY --from=maven /netty/target/netty-example-0.1-jar-with-dependencies.jar app.jar
CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+AggressiveOpts", "-jar", "app.jar"]
