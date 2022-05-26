FROM gradle:jdk18 as gradle
WORKDIR /sample
COPY sample/build.gradle.kts build.gradle.kts
COPY sample/src src
RUN gradle clean shadowJar --no-daemon

FROM openjdk:18-jdk-buster
WORKDIR /sample
COPY --from=gradle /sample/build/libs/sample-1.0.0-all.jar app.jar

EXPOSE 8080

CMD ["java", "-server", "-jar", "app.jar"]
