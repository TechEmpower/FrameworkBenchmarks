FROM gradle:4.7.0-jdk8 as gradle
USER root
WORKDIR /comsat
COPY build.gradle build.gradle
COPY src src
RUN gradle capsule -q

FROM openjdk:8-jre-slim
WORKDIR /comsat
COPY --from=gradle /comsat/build/libs/comsat-0.3-capsule.jar app.jar

EXPOSE 8080

CMD ["java", "-Dcapsule.mode=webactors-undertow", "-jar", "app.jar"]
