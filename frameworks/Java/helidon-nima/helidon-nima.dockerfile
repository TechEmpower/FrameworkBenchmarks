# Using corretto because there's no openjdk-19 maven image yet.

FROM maven:3-amazoncorretto-19 as maven
WORKDIR /helidon
COPY src src
COPY pom.xml pom.xml
RUN mvn package -q

FROM amazoncorretto:19
WORKDIR /helidon
COPY --from=maven /helidon/target/libs libs
COPY --from=maven /helidon/target/benchmark.jar app.jar

EXPOSE 8080

CMD java --enable-preview -server \
    -XX:+UseNUMA \
    -XX:+UseParallelGC \
    -jar app.jar