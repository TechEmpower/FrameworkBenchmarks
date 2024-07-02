FROM maven:3.9.6-eclipse-temurin-21 as maven
WORKDIR /spring
COPY src src
COPY pom.xml pom.xml
RUN mvn package -q

FROM eclipse-temurin:21.0.3_9-jre-jammy
WORKDIR /spring
COPY --from=maven /spring/target/spring-webflux-benchmark.jar app.jar

EXPOSE 8080

CMD ["java", "-server", "-XX:+UseNUMA", "-Dlogging.level.root=OFF", "-jar", "app.jar", "--spring.profiles.active=jdbc,postgres"]
