FROM maven:3-eclipse-temurin-25-alpine as maven
WORKDIR /httpserver
COPY pom.xml pom.xml
COPY src src
RUN mvn compile assembly:single -q

FROM amazoncorretto:25
WORKDIR /httpserver
COPY --from=maven /httpserver/target/httpserver-1.0-jar-with-dependencies.jar app.jar

EXPOSE 8080

CMD ["java", "-server", "-XX:MaxRAMPercentage=70", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-Dsun.net.httpserver.nodelay=true", "-jar", "app.jar"]
