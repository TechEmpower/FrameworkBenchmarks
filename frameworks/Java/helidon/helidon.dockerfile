FROM docker.io/maven:3.8.6-eclipse-temurin-19 as maven
WORKDIR /helidon
COPY reactive/src src
COPY reactive/pom.xml pom.xml
RUN mvn package -q

FROM openjdk:19-jdk-slim
WORKDIR /helidon
COPY --from=maven /helidon/target/libs libs
COPY --from=maven /helidon/target/benchmark-reactive.jar app.jar

EXPOSE 8080

CMD java -server \
    -Dio.netty.buffer.checkBounds=false \
    -Dio.netty.buffer.checkAccessible=false \
    -jar app.jar
