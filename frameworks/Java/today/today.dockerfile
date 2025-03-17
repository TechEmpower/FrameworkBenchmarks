FROM gradle:8.13.0-jdk21 as build
COPY --chown=gradle:gradle . /infra-src
WORKDIR /infra-src
RUN gradle installDist --no-daemon

#FROM openjdk:21
FROM bellsoft/liberica-openjre-debian:21.0.5
WORKDIR /today
COPY --from=build /infra-src/build/install/today/ ./

EXPOSE 8080
ENTRYPOINT "./bin/today"
