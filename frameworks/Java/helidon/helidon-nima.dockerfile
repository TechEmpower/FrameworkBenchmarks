FROM docker.io/maven:3.9.2-eclipse-temurin-20 as maven
WORKDIR /helidon
COPY nima/src src
COPY nima/pom.xml pom.xml
RUN mvn package -q

FROM openjdk:20-jdk-slim
WORKDIR /helidon
COPY --from=maven /helidon/target/libs libs
COPY --from=maven /helidon/target/benchmark-nima.jar app.jar

EXPOSE 8080

CMD java --enable-preview \
    -XX:+UseNUMA \
    -XX:+UseParallelGC \
    -XX:+UseStringDeduplication \
    -jar app.jar
