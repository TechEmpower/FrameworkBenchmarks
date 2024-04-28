FROM gradle:8.6.0-jdk17 as build
COPY --chown=gradle:gradle . /home/gradle/src
WORKDIR /home/gradle/src
RUN gradle installInfraDist --no-daemon

FROM openjdk:21
WORKDIR /today
COPY --from=build /home/gradle/src/build/install/today-infra-app/ ./

EXPOSE 8080
ENTRYPOINT "./bin/today"
