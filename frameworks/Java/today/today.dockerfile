FROM gradle:8.13-jdk21 as build
COPY --chown=gradle:gradle . /home/gradle/src
WORKDIR /home/gradle/src
RUN gradle installDist --no-daemon

FROM openjdk:21
WORKDIR /today
COPY --from=build /home/gradle/src/build/install/today/ ./

EXPOSE 8080
ENTRYPOINT "./bin/today"
