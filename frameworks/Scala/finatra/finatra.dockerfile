FROM techempower/sbt:0.1

ADD ./ /finatra
WORKDIR /finatra

RUN sbt clean assembly -batch

CMD java \
    -Dio.netty.recycler.maxCapacityPerThread=0 \
    -Dio.netty.leakDetection.level=disabled \
    -Dcom.twitter.util.events.sinkEnabled=false \
    -server \
    -XX:+UseNUMA \
    -XX:+UseParallelGC \
    -XX:+AggressiveOpts \
    -jar target/scala-2.12/finatra-benchmark.jar \
    -log.level=ERROR \
    -http.response.charset.enabled=false
