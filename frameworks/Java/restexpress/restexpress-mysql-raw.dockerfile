FROM maven:3.5.3-jdk-10 as maven
WORKDIR /restexpress
COPY src src
COPY pom.xml pom.xml
RUN mvn package -q

FROM openjdk:10-jre-slim
WORKDIR /restexpress
COPY --from=maven /restexpress/target/world-1.0-SNAPSHOT.jar app.jar
CMD ["java", "-jar", "app.jar"]
