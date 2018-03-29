FROM tfb/gradle:latest
ADD ./ /http4k
WORKDIR /http4k
RUN gradle clean build apache:uber
CMD java \
    -server \
    -XX:+UseNUMA \
    -XX:+UseParallelGC \
    -XX:+AggressiveOpts \
    -XX:+AlwaysPreTouch \
    -jar apache/build/libs/http4k-apache-benchmark.jar
