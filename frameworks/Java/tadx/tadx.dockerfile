#FROM gradle:8.14.3-jdk21 as build
#COPY --chown=gradle:gradle . /home/gradle/tadx-benchmark
#WORKDIR /home/gradle/tadx-benchmark
#RUN gradle build -x test --no-daemon

#FROM gradle:8.14.3-jdk21
# maven:3.9.9-eclipse-temurin-24-noble

FROM eclipse-temurin:21-jdk
WORKDIR /tadx
#COPY --from=build /home/gradle/tadx-benchmark/build/libs/tadx-benchmark-0.0.1-SNAPSHOT.jar tadx.jar
COPY ./build/libs/tadx-benchmark-0.0.1-SNAPSHOT.jar tadx.jar

EXPOSE 8000

CMD java $JAVA_OPTIONS                  \
      -server -Xms2G -Xmx2G             \
      -XX:+UseParallelGC                \
      -XX:+UseNUMA                      \
          -Dvertx.disableMetrics=true                       \
          -Dvertx.disableWebsockets=true                    \
          -Dvertx.flashPolicyHandler=false                  \
          -Dvertx.threadChecks=false                        \
          -Dvertx.disableContextTimings=true                \
          -Dvertx.disableTCCL=true                          \
          -Dvertx.cacheImmutableHttpResponseHeaders=true    \
          -Dvertx.disableHttpHeadersValidation=true         \
          -Dio.netty.buffer.checkBounds=false               \
          -Dio.netty.buffer.checkAccessible=false           \
          -Djava.lang.Integer.IntegerCache.high=10000       \
          -Dfile.encoding=UTF-8                             \
          -Djdk.trackAllThreads=false                       \
      -jar tadx.jar
