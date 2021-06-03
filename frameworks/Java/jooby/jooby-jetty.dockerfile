FROM maven:3.6.1-jdk-11-slim as maven
WORKDIR /jooby
COPY pom.xml pom.xml
COPY src src
COPY public public
RUN mvn package -q -P jetty

FROM openjdk:11.0.3-jdk-slim
WORKDIR /jooby
COPY --from=maven /jooby/target/jooby.jar app.jar
COPY conf conf

EXPOSE 8080

CMD ["java", "-server", "-Xms4g", "-Xmx4g", "-XX:+AggressiveOpts", "-XX:-UseBiasedLocking", "-XX:+UseStringDeduplication", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-jar", "app.jar"]
