FROM maven:3.9.7-amazoncorretto-21 as maven
WORKDIR /smart-socket
COPY pom.xml pom.xml
COPY src src
RUN mvn install assembly:single -q

FROM openjdk:21-jdk-slim
WORKDIR /smart-socket
COPY --from=maven /smart-socket/target/smart-socket-benchmark-1.0-jar-with-dependencies.jar app.jar

EXPOSE 8080

CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-cp", "app.jar", "org.smartboot.http.Bootstrap"]
