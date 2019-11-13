FROM maven:3.6.1-jdk-11-slim as maven
WORKDIR /jooby2
COPY pom.xml pom.xml
COPY src src
COPY public public
RUN mvn package -q -P undertow

FROM openjdk:11.0.3-jdk-slim
WORKDIR /jooby2
COPY --from=maven /jooby2/target/jooby-2x.jar app.jar
COPY conf conf
CMD ["java", "-server", "-Xms4g", "-Xmx4g", "-XX:+AggressiveOpts", "-XX:-UseBiasedLocking", "-XX:+UseStringDeduplication", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-jar", "app.jar"]
