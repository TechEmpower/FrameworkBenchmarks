FROM techempower/java:0.1

ADD ./ /http4k
WORKDIR /http4k
RUN gradle clean build jetty:uber
CMD java \
    -server \
    -XX:+UseNUMA \
    -XX:+UseParallelGC \
    -XX:+AggressiveOpts \
    -XX:+AlwaysPreTouch \
    -jar jetty/build/libs/http4k-jetty-benchmark.jar
