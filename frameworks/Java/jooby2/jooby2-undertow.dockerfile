FROM maven:3.5.3-jdk-8-slim as maven
WORKDIR /jooby2
COPY pom.xml pom.xml
COPY src src
COPY public public
RUN mvn package -q -P undertow

FROM openjdk:8-jre-slim
WORKDIR /jooby2
COPY --from=maven /jooby2/target/jooby-2x.jar app.jar
COPY conf conf
CMD ["java", "-server", "-Xms512m", "-Xmx2g", "-jar", "app.jar"]
