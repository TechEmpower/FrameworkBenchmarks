#FROM gradle:8.14.3-jdk21 as build
#COPY --chown=gradle:gradle . /home/gradle/tadx-benchmark
#WORKDIR /home/gradle/tadx-benchmark
#RUN gradle build -x test --no-daemon

#FROM gradle:8.14.3-jdk21
# maven:3.9.9-eclipse-temurin-24-noble

FROM maven:3.9.9-eclipse-temurin-24-noble
WORKDIR /tadx
#COPY --from=build /home/gradle/tadx-benchmark/build/libs/tadx-benchmark-0.0.1-SNAPSHOT.jar tadx.jar
COPY ./build/libs/tadx-benchmark-0.0.1-SNAPSHOT.jar tadx.jar

EXPOSE 8080

CMD java $JAVA_OPTIONS                  \
      --enable-native-access=ALL-UNNAMED \
      --sun-misc-unsafe-memory-access=allow \
      --add-opens=java.base/java.lang=ALL-UNNAMED \
      -server -Xms2G                    \
      -XX:+UseParallelGC                \
      -XX:+UseNUMA                      \
      -XX:+UnlockDiagnosticVMOptions \
      -XX:+DebugNonSafepoints \
      -Dvertx.disableMetrics=true                       \
      -Dvertx.disableWebsockets=true                    \
      -Dvertx.flashPolicyHandler=false                  \
      -Dvertx.threadChecks=false                        \
      -Dvertx.disableContextTimings=true                \
      -Dvertx.disableTCCL=true                          \
      -Dvertx.cacheImmutableHttpResponseHeaders=true    \
      -Dvertx.disableHttpHeadersValidation=true         \
      -Dvertx.eventLoopPoolSize=$((`grep --count ^processor /proc/cpuinfo`)) \
      -Dio.netty.noUnsafe=false \
      -Dio.netty.buffer.checkBounds=false               \
      -Dio.netty.buffer.checkAccessible=false           \
      -Dio.netty.iouring.ringSize=16384 \
      -Djdk.trackAllThreads=false                       \
      -Djava.lang.Integer.IntegerCache.high=10000       \
      -Dfile.encoding=UTF-8                             \
      -jar tadx.jar
