FROM maven:3.9.11-eclipse-temurin-25-alpine as maven
WORKDIR /armeria
COPY src src
COPY pom.xml pom.xml
RUN mvn package -q

FROM eclipse-temurin:25-jre-alpine
WORKDIR /armeria
COPY --from=maven /armeria/target/hello-1.0-SNAPSHOT.jar app.jar

EXPOSE 8080

CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-jar", "app.jar"]
