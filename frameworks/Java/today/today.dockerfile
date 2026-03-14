FROM gradle:9.3.1-jdk25 as build
COPY --chown=gradle:gradle . /infra-src
WORKDIR /infra-src
RUN gradle installDist --no-daemon

FROM bellsoft/liberica-openjre-debian:25.0.2
RUN apt install findutils
WORKDIR /today
COPY --from=build /infra-src/build/install/today/ ./

EXPOSE 8080
ENTRYPOINT "./bin/today"
