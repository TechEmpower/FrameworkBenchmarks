FROM maven:3.9.5-eclipse-temurin-21 as maven
WORKDIR /spring
COPY src src
COPY pom.xml pom.xml
RUN mvn package -q

FROM eclipse-temurin:21
WORKDIR /spring
COPY --from=maven /spring/target/spring-webflux-benchmark.jar app.jar

EXPOSE 8080

CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-Dlogging.level.root=OFF", "-jar", "app.jar", "--spring.profiles.active=r2dbc,postgres"]
