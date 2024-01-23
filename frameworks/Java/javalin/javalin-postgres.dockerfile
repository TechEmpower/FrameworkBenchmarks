FROM maven:3.6.3-jdk-11-slim as maven
WORKDIR /javalin
COPY src src
COPY pom.xml pom.xml
RUN mvn clean package -q

FROM openjdk:11.0.10-jdk-slim
WORKDIR /javalin
COPY --from=maven /javalin/target/javalin-1.0-shaded.jar app.jar

ARG BENCHMARK_ENV

ENV BENCHMARK_ENV=$BENCHMARK_ENV

EXPOSE 8080

CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-Dlogging.level.root=OFF", "-jar", "app.jar"]