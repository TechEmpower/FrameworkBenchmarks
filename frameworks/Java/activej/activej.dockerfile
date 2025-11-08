FROM maven:3.9.0-eclipse-temurin-17 as maven
WORKDIR /activej
COPY pom.xml pom.xml
COPY src src
RUN mvn compile assembly:single -q

FROM amazoncorretto:25
WORKDIR /activej
COPY --from=maven /activej/target/activej-server-benchmark-0.0.1-SNAPSHOT-jar-with-dependencies.jar app.jar
EXPOSE 8080
CMD ["java", "-server", "-XX:+UseParallelGC", "-jar", "app.jar"]