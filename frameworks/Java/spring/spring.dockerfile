FROM maven:3.5.3-jdk-10-slim as maven
WORKDIR /spring
COPY src src
COPY pom.xml pom.xml
RUN mvn package -q

FROM openjdk:10-jre-slim
WORKDIR /spring
COPY --from=maven /spring/target/hello-spring-1.0-SNAPSHOT.jar app.jar
CMD ["java", "-jar", "app.jar"]
