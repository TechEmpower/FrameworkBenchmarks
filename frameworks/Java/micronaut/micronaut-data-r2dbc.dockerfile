FROM gradle:9.3.1-jdk25 as build
COPY --chown=gradle:gradle . /home/gradle/src
WORKDIR /home/gradle/src
RUN gradle micronaut-data-r2dbc:build -x test -x internalStartTestResourcesService --no-daemon

FROM container-registry.oracle.com/java/openjdk:25.0.2
WORKDIR /micronaut
COPY --from=build /home/gradle/src/micronaut-data-r2dbc/build/libs/micronaut-data-r2dbc-all.jar micronaut.jar
COPY run_benchmark.sh run_benchmark.sh

EXPOSE 8080
ENTRYPOINT "./run_benchmark.sh"
