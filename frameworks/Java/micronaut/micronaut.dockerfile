FROM gradle:8.14.3-jdk21 as build
COPY --chown=gradle:gradle . /home/gradle/src
WORKDIR /home/gradle/src
RUN gradle micronaut-vertx-pg-client:build -x test -x internalStartTestResourcesService --no-daemon

FROM openjdk:24
WORKDIR /micronaut
COPY --from=build /home/gradle/src/micronaut-vertx-pg-client/build/libs/micronaut-vertx-pg-client-all.jar micronaut.jar
COPY run_benchmark.sh run_benchmark.sh

EXPOSE 8080
ENTRYPOINT "./run_benchmark.sh"
