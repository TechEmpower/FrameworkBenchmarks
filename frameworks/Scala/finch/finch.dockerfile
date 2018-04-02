FROM techempower/sbt:0.1

ADD ./ /finch
WORKDIR /finch

RUN sbt assembly -batch

CMD java \
    -server \
    -XX:+UseNUMA \
    -XX:+UseParallelGC \
    -XX:+AggressiveOpts \
    -Dio.netty.recycler.maxCapacityPerThread=0 \
    -Dio.netty.leakDetection.level=disabled \
    -jar target/scala-2.12/techempower-benchmarks-finch-assembly-1.0.jar
