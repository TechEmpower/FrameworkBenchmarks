FROM maven:3.6.1-jdk-11-slim as maven
WORKDIR /jooby
COPY pom.xml pom.xml
COPY src src
COPY public public
RUN mvn package -q

FROM openjdk:11.0.3-jdk-slim
WORKDIR /jooby
COPY --from=maven /jooby/target/jooby-1.0.jar app.jar
COPY conf conf
CMD ["java", "-server", "-Xms512m", "-Xmx2g", "-jar", "app.jar"]
