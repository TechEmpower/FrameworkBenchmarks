FROM techempower/sbt:0.1

COPY ./ ./

RUN sbt 'oneJar' -batch

CMD java -server -XX:+UseNUMA -XX:+UseParallelGC -XX:+AggressiveOpts -Dio.netty.recycler.maxCapacityPerThread=0 -Dio.netty.leakDetection.level=disabled -jar target/scala-2.11/*finagle*one-jar.jar
