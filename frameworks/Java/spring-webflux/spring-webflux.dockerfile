FROM maven:3.5.3-jdk-10-slim as maven
WORKDIR /spring
COPY src src
COPY pom.xml pom.xml
RUN mvn package -q

FROM openjdk:10-jre-slim
WORKDIR /spring
COPY --from=maven /spring/target/spring-webflux-benchmark-1.0-SNAPSHOT.jar app.jar
CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-Dlogging.level.root=OFF", "-jar", "app.jar"]
