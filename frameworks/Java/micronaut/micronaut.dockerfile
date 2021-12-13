FROM gradle:6.9-jdk11 as build
WORKDIR /micronaut
COPY src src
COPY build.gradle build.gradle
COPY settings.gradle settings.gradle
RUN gradle build buildLayers --no-daemon

FROM openjdk:11-jre-slim
WORKDIR /micronaut
COPY --from=build /micronaut/build/docker/layers/libs /home/app/libs
COPY --from=build /micronaut/build/docker/layers/resources /home/app/resources
COPY --from=build /micronaut/build/docker/layers/application.jar /home/app/application.jar

EXPOSE 8080

CMD ["java", "-server", "-XX:+UseNUMA", "-XX:+UseParallelGC", "-Dmicronaut.environments=benchmark", "-Dlog-root-level=OFF", "-jar", "/home/app/application.jar"]
