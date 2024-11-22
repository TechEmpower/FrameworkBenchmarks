FROM jelastic/maven:3.9.5-openjdk-21 as maven
WORKDIR /httpserver-robaho
COPY pom.xml pom.xml
COPY src src
RUN mvn compile assembly:single -q

FROM openjdk:21-jdk-slim
WORKDIR /httpserver-robaho
COPY --from=maven /httpserver-robaho/target/httpserver-robaho-1.0-jar-with-dependencies.jar app.jar

EXPOSE 8080

CMD ["java", "-server", "-jar", "app.jar"]