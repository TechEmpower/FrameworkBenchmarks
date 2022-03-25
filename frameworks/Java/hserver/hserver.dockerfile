FROM maven:3.6.3-openjdk-8-slim as maven
WORKDIR /hserver
COPY pom.xml pom.xml
COPY src src
RUN mvn package

FROM openjdk:8u275-jdk-slim
WORKDIR /hserver
COPY --from=maven /hserver/target/hserver-1.0.jar app.jar

EXPOSE 8888

CMD ["java", "-jar", "app.jar"]