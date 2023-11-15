FROM gradle:8.4.0-jdk21 as gradle
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
FROM ghcr.io/graalvm/graalvm-community:21.0.0-ol9-20230919 as graalvm
COPY --from=gradle /http4k/core/src/main/resources/* /home/app/http4k-apache-graalvm/
COPY --from=gradle /http4k/apache-graalvm/build/libs/http4k-benchmark.jar /home/app/http4k-apache-graalvm/
COPY --from=gradle /http4k/apache-graalvm/config/*.json /home/app/http4k-apache-graalvm/
WORKDIR /home/app/http4k-apache-graalvm
RUN native-image \
    -H:ReflectionConfigurationFiles=reflect-config.json \
    -H:ResourceConfigurationFiles=resource-config.json \
    --initialize-at-build-time="org.slf4j.LoggerFactory,org.slf4j.simple.SimpleLogger,org.slf4j.impl.StaticLoggerBinder" \
    --no-fallback -cp http4k-benchmark.jar Http4kGraalVMBenchmarkServerKt

EXPOSE 9000
ENTRYPOINT ["/home/app/http4k-apache-graalvm/http4kgraalvmbenchmarkserverkt"]