FROM maven:3.5.3-jdk-9-slim as maven
WORKDIR /spring
COPY src src
COPY pom.xml pom.xml
RUN mvn package -q

FROM openjdk:9-jre-slim
WORKDIR /spring
COPY --from=maven /spring/target/spring.war app.jar
CMD ["java", "-jar", "app.jar"]
