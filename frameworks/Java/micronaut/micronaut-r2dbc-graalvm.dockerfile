FROM ghcr.io/graalvm/graalvm-ce:ol7-java17-22.3.0 as build
COPY . /home/gradle/src
WORKDIR /home/gradle/src
RUN ./gradlew  --no-daemon
RUN ./gradlew micronaut-r2dbc:nativeBuild -x test --no-daemon

FROM frolvlad/alpine-glibc:alpine-3.12
RUN apk --no-cache update && apk add libstdc++
WORKDIR /micronaut
COPY --from=build /home/gradle/src/micronaut-r2dbc/build/native/nativeCompile/micronaut-r2dbc micronaut

EXPOSE 8080
ENV MICRONAUT_ENVIRONMENTS=benchmark
ENTRYPOINT "./micronaut"
