FROM gradle:7.6-jdk17 as gradle
WORKDIR /vertx-web-kotlin-coroutines
COPY src src
COPY build.gradle.kts build.gradle.kts
COPY gradle.properties gradle.properties
COPY settings.gradle.kts settings.gradle.kts
RUN gradle shadowJar

EXPOSE 8080

CMD java \
    -server                                           \
    -XX:+UseNUMA                                      \
    -XX:+UseParallelGC                                \
    -Dvertx.disableMetrics=true                       \
    -Dvertx.disableH2c=true                           \
    -Dvertx.disableWebsockets=true                    \
    -Dvertx.flashPolicyHandler=false                  \
    -Dvertx.threadChecks=false                        \
    -Dvertx.disableContextTimings=true                \
    -Dvertx.disableTCCL=true                          \
    -Dvertx.disableHttpHeadersValidation=true         \
    -Dvertx.eventLoopPoolSize=$((`grep --count ^processor /proc/cpuinfo`)) \
    -Dio.netty.buffer.checkBounds=false               \
    -Dio.netty.buffer.checkAccessible=false           \
    -jar                                              \
    build/libs/vertx-web-kotlin-coroutines-benchmark-4.3.8-fat.jar \
    --instances                                       \
    `grep --count ^processor /proc/cpuinfo`           \
    --conf                                            \
    src/main/conf/config.json                         \
    --options                                         \
    src/main/conf/vertx.json
