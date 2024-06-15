FROM ghcr.io/graalvm/native-image-community:21-ol9 as build
RUN microdnf install findutils # Gradle 8.7 requires xargs
COPY . /home/gradle/src
WORKDIR /home/gradle/src
RUN ./gradlew micronaut-vertx-pg-client:nativeCompile -x test -x internalStartTestResourcesService --no-daemon

FROM cgr.dev/chainguard/wolfi-base:latest
RUN apk --no-cache update && apk add libstdc++
WORKDIR /micronaut
COPY --from=build /home/gradle/src/micronaut-vertx-pg-client/build/native/nativeCompile/micronaut-vertx-pg-client micronaut

EXPOSE 8080
ENV MICRONAUT_ENVIRONMENTS=benchmark
ENTRYPOINT "./micronaut"
