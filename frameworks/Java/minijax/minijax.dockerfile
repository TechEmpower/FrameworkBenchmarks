FROM maven:3.6.1-jdk-11-slim as maven
WORKDIR /minijax
COPY pom.xml pom.xml
COPY src src
RUN mvn package -q

FROM openjdk:11.0.3-jdk-slim
WORKDIR /minijax
COPY --from=maven /minijax/target/minijax-techempower-0.0.1.jar app.jar
COPY minijax.properties minijax.properties

EXPOSE 8080

CMD ["java", "-server", "-Xms512m", "-Xmx2g", "-XX:+AggressiveOpts", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-jar", "app.jar"]
