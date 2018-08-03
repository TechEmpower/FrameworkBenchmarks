FROM gradle:4.7.0-jdk8 as gradle
USER root
WORKDIR /ratpack
COPY build.gradle build.gradle
COPY src src
RUN gradle shadowJar

FROM openjdk:8-jre-slim
WORKDIR /ratpack
COPY --from=gradle /ratpack/build/libs/ratpack-all.jar app.jar
CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-jar", "app.jar"]
