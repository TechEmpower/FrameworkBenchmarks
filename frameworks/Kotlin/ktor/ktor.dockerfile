FROM maven:3.5.3-jdk-10-slim as maven
WORKDIR /ktor
COPY ktor/pom.xml pom.xml
COPY ktor/src src
RUN mvn clean package -q

FROM openjdk:10-jre-slim
WORKDIR /ktor
COPY --from=maven /ktor/target/tech-empower-framework-benchmark-1.0-SNAPSHOT-netty-bundle.jar app.jar
CMD ["java", "-server","-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+AggressiveOpts", "-XX:+AlwaysPreTouch", "-jar", "app.jar"]
