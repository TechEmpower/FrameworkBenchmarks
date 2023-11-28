FROM ghcr.io/graalvm/graalvm-community:latest as build
COPY . /home/gradle/src
WORKDIR /home/gradle/src
RUN ./gradlew  --no-daemon
RUN ./gradlew micronaut-data-jdbc:nativeBuild -x test --no-daemon

FROM frolvlad/alpine-glibc:glibc-2.34
RUN apk --no-cache update && apk add libstdc++
WORKDIR /micronaut
COPY --from=build /home/gradle/src/micronaut-data-jdbc/build/native/nativeCompile/micronaut-data-jdbc micronaut

EXPOSE 8080
ENV MICRONAUT_ENVIRONMENTS=benchmark
ENTRYPOINT "./micronaut"
