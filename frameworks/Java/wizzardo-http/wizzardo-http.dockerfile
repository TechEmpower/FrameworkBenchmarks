FROM gradle:5.4.1-jdk11 as gradle

USER root
WORKDIR /wizzardo-http

COPY build.gradle build.gradle
COPY src src

RUN gradle --refresh-dependencies clean fatJar

FROM openjdk:11.0.3-jdk-slim
WORKDIR /wizzardo-http
COPY --from=gradle /wizzardo-http/build/libs/wizzardo-http-all-1.0-SNAPSHOT.jar app.jar

EXPOSE 8080

CMD ["java", "-Xmx2G", "-Xms2G", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-XX:+AggressiveOpts", "-jar", "app.jar", "env=prod"]
