FROM maven:3.9.7-amazoncorretto-21 as maven
WORKDIR /solon
COPY pom.xml pom.xml
COPY src src
RUN mvn compile assembly:single -q

FROM openjdk:21-jdk-slim
WORKDIR /solon
COPY --from=maven /solon/target/hello-solon.jar app.jar

EXPOSE 8080

CMD ["java", "-server", "-cp", "app.jar", "hello.Main"]