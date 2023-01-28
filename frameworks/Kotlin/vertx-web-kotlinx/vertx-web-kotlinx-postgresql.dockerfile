FROM gradle:7.6-jdk17

WORKDIR /vertx-web-kotlinx
COPY build.gradle.kts build.gradle.kts
COPY settings.gradle.kts settings.gradle.kts
COPY gradle.properties gradle.properties
COPY src src
RUN gradle assembleDist

EXPOSE 8080

# TODO: is running a shadow jar faster?
# TODO: do the extra parameters in the "vertx-web" benchmark improve performance?
CMD tar -xf build/distributions/vertx-web-kotlinx-benchmark.tar && vertx-web-kotlinx-benchmark/bin/vertx-web-kotlinx-benchmark
