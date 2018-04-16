FROM maven:3.5.3-jdk-10-slim as maven
WORKDIR /minijax
COPY pom.xml pom.xml
COPY src src
RUN mvn package -q

FROM openjdk:10-jre-slim
WORKDIR /minijax
COPY --from=maven /minijax/target/minijax-techempower-0.0.1.jar app.jar
COPY minijax.properties minijax.properties
CMD ["java", "-server", "-Xms512m", "-Xmx2g", "-XX:+AggressiveOpts", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-jar", "app.jar"]
