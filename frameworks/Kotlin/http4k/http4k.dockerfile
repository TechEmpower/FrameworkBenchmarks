FROM tfb/java:latest
ADD ./ /http4k
WORKDIR /http4k
RUN ./gradlew clean build jetty:uber
CMD java \
    -server \
    -XX:+UseNUMA \
    -XX:+UseParallelGC \
    -XX:+AggressiveOpts \
    -XX:+AlwaysPreTouch \
    -jar jetty/build/libs/http4k-jetty-benchmark.jar
