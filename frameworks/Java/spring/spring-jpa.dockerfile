FROM maven:3.9.6-eclipse-temurin-21 as maven
RUN mvn -version
WORKDIR /spring
COPY src src
COPY pom.xml pom.xml
RUN mvn package -q

FROM eclipse-temurin:21.0.3_9-jre-jammy
RUN java -version
WORKDIR /spring
COPY --from=maven /spring/target/hello-spring-1.0-SNAPSHOT.jar app.jar

EXPOSE 8080

CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseG1GC", "-XX:+DisableExplicitGC", "-XX:+UseStringDeduplication", "-Dlogging.level.root=OFF", "-jar", "app.jar", "--spring.profiles.active=jpa"]
