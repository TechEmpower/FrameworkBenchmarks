FROM maven:3.8.5-openjdk-17-slim as maven
WORKDIR /spring
COPY src src
COPY pom.xml pom.xml
RUN mvn package -q

FROM openjdk:17.0-jdk-slim
WORKDIR /spring
COPY --from=maven /spring/target/spring-webflux-benchmark.jar app.jar

EXPOSE 8080

CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-Dlogging.level.root=OFF", "-jar", "app.jar", "--spring.profiles.active=jdbc,postgres"]
