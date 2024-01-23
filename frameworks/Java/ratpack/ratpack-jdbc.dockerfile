FROM gradle:5.4.1-jdk11 as gradle
USER root
WORKDIR /ratpack
COPY build.gradle build.gradle
COPY src src
RUN gradle shadowJar

FROM openjdk:11.0.3-jdk-slim
WORKDIR /ratpack
COPY --from=gradle /ratpack/build/libs/ratpack-all.jar app.jar

EXPOSE 5050

CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-jar", "app.jar", "profile.name=jdbc"]
