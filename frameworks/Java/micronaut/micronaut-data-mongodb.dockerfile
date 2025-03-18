FROM gradle:8.7.0-jdk17 as build
COPY --chown=gradle:gradle . /home/gradle/src
WORKDIR /home/gradle/src
RUN gradle micronaut-data-mongodb:build -x test -x internalStartTestResourcesService --no-daemon

FROM openjdk:23
WORKDIR /micronaut
COPY --from=build /home/gradle/src/micronaut-data-mongodb/build/libs/micronaut-data-mongodb-all.jar micronaut.jar
COPY run_benchmark.sh run_benchmark.sh

EXPOSE 8080
ENTRYPOINT "./run_benchmark.sh"
