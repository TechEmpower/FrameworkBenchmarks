FROM gradle:8.7.0-jdk17 as build
COPY --chown=gradle:gradle . /home/gradle/src
WORKDIR /home/gradle/src
RUN gradle micronaut-r2dbc:build -x test -x internalStartTestResourcesService --no-daemon

FROM openjdk:21
WORKDIR /micronaut
COPY --from=build /home/gradle/src/micronaut-r2dbc/build/libs/micronaut-r2dbc-all.jar micronaut.jar
COPY run_benchmark.sh run_benchmark.sh

EXPOSE 8080
ENTRYPOINT "./run_benchmark.sh"
