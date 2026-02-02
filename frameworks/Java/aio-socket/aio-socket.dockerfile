FROM maven:3-eclipse-temurin-25-alpine as maven
WORKDIR /app
COPY pom.xml pom.xml
COPY src src
RUN mvn compile assembly:single -q

FROM amazoncorretto:25
WORKDIR /app
COPY --from=maven /app/target/aio-socket-benchmark-1.0-jar-with-dependencies.jar /app/app.jar

EXPOSE 8080

CMD ["java", "-server", "-XX:MaxRAMPercentage=70", "-XX:+UseParallelGC", "-jar", "app.jar"]
