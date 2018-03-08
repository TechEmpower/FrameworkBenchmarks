FROM tfb/java:latest
ADD ./ /http4k
WORKDIR /http4k
RUN ./gradlew clean build sunhttp:uber
CMD java \
    -server \
    -XX:+UseNUMA \
    -XX:+UseParallelGC \
    -XX:+AggressiveOpts \
    -XX:+AlwaysPreTouch \
    -jar sunhttp/build/libs/http4k-sunhttp-benchmark.jar
