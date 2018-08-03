FROM maven:3.5.3-jdk-10-slim as maven
WORKDIR /undertow
COPY pom.xml pom.xml
COPY src src
RUN mvn clean package -q

FROM openjdk:10-jre-slim
WORKDIR /undertow
COPY --from=maven /undertow/target/app.jar app.jar
CMD ["java", "-jar", "app.jar", "POSTGRESQL"]
