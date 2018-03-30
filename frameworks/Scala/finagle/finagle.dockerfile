FROM tfb/sbt:latest

ADD ./ /finagle
WORKDIR /finagle

RUN sbt assembly -batch

CMD java \
    -server \
    -XX:+UseNUMA \
    -XX:+UseParallelGC \
    -XX:+AggressiveOpts \
    -Dio.netty.recycler.maxCapacityPerThread=0 \
    -Dio.netty.leakDetection.level=disabled \
    -jar target/scala-2.12/finagle-assembly-1.0.jar
