FROM tfb/java:latest
ADD ./ /http4k
WORKDIR /http4k
RUN ./gradlew clean build apache:uber
CMD java \
    -server \
    -XX:+UseNUMA \
    -XX:+UseParallelGC \
    -XX:+AggressiveOpts \
    -XX:+AlwaysPreTouch \
    -jar apache/build/libs/http4k-apache-benchmark.jar
