FROM maven:3.5.3-jdk-10 as maven
WORKDIR /restexpress
COPY src src
COPY src/main/resources/config/dev/mysql-environment.properties src/main/resources/config/dev/environment.properties
COPY pom.xml pom.xml
RUN mvn package -q

FROM openjdk:10-jre-slim
WORKDIR /restexpress
COPY --from=maven /restexpress/target/world-1.0-SNAPSHOT.jar app.jar
CMD ["java", "-jar", "app.jar"]
