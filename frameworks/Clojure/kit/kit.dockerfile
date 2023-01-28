# syntax = docker/dockerfile:1.2
FROM clojure:openjdk-17 AS build

WORKDIR /
COPY . /

RUN clj -Sforce -T:build all

FROM azul/zulu-openjdk-alpine:17

COPY --from=build /target/te-bench-standalone.jar /te-bench/te-bench-standalone.jar

EXPOSE 8080

ENV PORT=8080
ENV JAVA_OPTS="-XX:+UseContainerSupport -Dfile.encoding=UTF-8"
ENV JDBC_URL="jdbc:postgresql://tfb-database/hello_world?user=benchmarkdbuser&password=benchmarkdbpass"

ENTRYPOINT exec java $JAVA_OPTS -jar /te-bench/te-bench-standalone.jar
