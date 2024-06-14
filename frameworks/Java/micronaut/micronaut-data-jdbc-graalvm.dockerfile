FROM ghcr.io/graalvm/native-image-community:21-ol9 as build
RUN microdnf install findutils # Gradle 8.7 requires xargs
COPY . /home/gradle/src
WORKDIR /home/gradle/src
RUN ./gradlew micronaut-data-jdbc:nativeCompile -x test -x internalStartTestResourcesService --no-daemon

FROM cgr.dev/chainguard/wolfi-base:latest
WORKDIR /micronaut
COPY --from=build /home/gradle/src/micronaut-data-jdbc/build/native/nativeCompile/micronaut-data-jdbc micronaut

EXPOSE 8080
ENV MICRONAUT_ENVIRONMENTS=benchmark
ENTRYPOINT "./micronaut"
