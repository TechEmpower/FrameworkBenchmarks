FROM maven:3.5.3-jdk-10-slim as maven
WORKDIR /baratine
COPY pom.xml pom.xml
COPY src src
RUN mvn package -q

FROM openjdk:10-jre-slim
WORKDIR /baratine
CMD ["java", "-jar", "target/testTechempowerBaratine-0.0.1-SNAPSHOT.jar", "tfb-database"]
