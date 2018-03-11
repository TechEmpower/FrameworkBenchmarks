FROM tfb/java:latest
ADD ./ /http4k
WORKDIR /http4k
RUN ./gradlew clean build undertow:uber
CMD java \
    -server \
    -XX:+UseNUMA \
    -XX:+UseParallelGC \
    -XX:+AggressiveOpts \
    -XX:+AlwaysPreTouch \
    -jar undertow/build/libs/http4k-undertow-benchmark.jar
