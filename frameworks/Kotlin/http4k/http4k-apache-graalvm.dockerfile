FROM gradle:8.7.0-jdk21 as gradle
USER root
WORKDIR /http4k
COPY build.gradle.kts build.gradle.kts
COPY settings.gradle.kts settings.gradle.kts
COPY apache apache
COPY core core
COPY core-jdbc core-jdbc
COPY core-pgclient core-pgclient
COPY apache-graalvm apache-graalvm

RUN gradle --quiet --no-daemon apache-graalvm:shadowJar

FROM ghcr.io/graalvm/native-image-community:21 as graalvm
COPY --from=gradle /http4k/core/src/main/resources/* /home/app/http4k-apache-graalvm/
COPY --from=gradle /http4k/apache-graalvm/build/libs/http4k-benchmark.jar /home/app/http4k-apache-graalvm/
COPY --from=gradle /http4k/apache-graalvm/config/*.json /home/app/http4k-apache-graalvm/

RUN native-image \
    --static --no-fallback \
    -H:+UnlockExperimentalVMOptions \
    -H:ReflectionConfigurationFiles=/home/app/http4k-apache-graalvm/reflect-config.json \
    -H:ResourceConfigurationFiles=/home/app/http4k-apache-graalvm/resource-config.json \
    --initialize-at-build-time="org.slf4j.LoggerFactory,org.slf4j.simple.SimpleLogger,org.slf4j.impl.StaticLoggerBinder" \
    -cp /home/app/http4k-apache-graalvm/http4k-benchmark.jar Http4kGraalVMBenchmarkServerKt

FROM frolvlad/alpine-glibc
COPY --from=graalvm /app/http4kgraalvmbenchmarkserverkt /http4kgraalvmbenchmarkserverkt
EXPOSE 9000
ENTRYPOINT ["/http4kgraalvmbenchmarkserverkt"]