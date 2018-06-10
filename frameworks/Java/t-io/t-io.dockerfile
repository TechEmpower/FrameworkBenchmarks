FROM maven:3.5.3-jdk-10-slim as maven
WORKDIR /t-io
COPY pom.xml pom.xml
COPY src src
RUN mvn compile assembly:single -q

FROM openjdk:10-jre-slim
WORKDIR /t-io
COPY --from=maven /t-io/target/tio-http-server-benchmark-3.0.2.v20180612-RELEASE.jar app.jar
CMD ["java", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-jar", "app.jar"]
