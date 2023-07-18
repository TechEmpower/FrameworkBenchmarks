FROM ghcr.io/graalvm/graalvm-community:latest as build
RUN microdnf install findutils
COPY . /home/gradle/src
WORKDIR /home/gradle/src
RUN ./gradlew  --no-daemon
RUN ./gradlew micronaut-r2dbc:nativeBuild -x test --no-daemon

FROM frolvlad/alpine-glibc:glibc-2.34
RUN apk --no-cache update && apk add libstdc++
WORKDIR /micronaut
COPY --from=build /home/gradle/src/micronaut-r2dbc/build/native/nativeCompile/micronaut-r2dbc micronaut

EXPOSE 8080
ENV MICRONAUT_ENVIRONMENTS=benchmark
ENTRYPOINT "./micronaut"
