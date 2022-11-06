FROM gradle:7.5.1-jdk11 as gradle
USER root
WORKDIR /http4k
COPY build.gradle build.gradle
COPY settings.gradle settings.gradle
COPY apache apache
COPY graalvm graalvm
COPY core core
RUN gradle --quiet graalvm:shadowJar

FROM ghcr.io/graalvm/graalvm-ce:ol7-java17-22.3.0 as graalvm
RUN gu install native-image

COPY --from=gradle /http4k/graalvm/build/libs/http4k-graalvm-benchmark.jar /home/app/http4k-graalvm/
COPY --from=gradle /http4k/graalvm/reflect-config.json /home/app/http4k-graalvm/reflect-config.json

WORKDIR /home/app/http4k-graalvm

#RUN $JAVA_HOME/bin/java -agentlib:native-image-agent=config-output-dir=META-INF/native-image -cp http4k-graalvm-benchmark.jar http4k.Http4kGraalVMBenchmarkServerKt || :
#RUN ls -al META-INF/native-image
#RUN cat META-INF/native-image/reflect-config.json

RUN native-image \
    -H:ReflectionConfigurationFiles=reflect-config.json \
    --initialize-at-build-time=org.slf4j.LoggerFactory \
    --initialize-at-build-time=org.slf4j.simple.SimpleLogger \
    --initialize-at-build-time=org.slf4j.impl.StaticLoggerBinder \
    --no-fallback -cp http4k-graalvm-benchmark.jar http4k.Http4kGraalVMBenchmarkServerKt

FROM frolvlad/alpine-glibc
RUN apk update && apk add libstdc++
EXPOSE 9000
COPY --from=graalvm /home/app/http4k-graalvm/http4k.http4kgraalvmbenchmarkserverkt /app/http4k-graalvm
ENTRYPOINT ["/app/http4k-graalvm"]
