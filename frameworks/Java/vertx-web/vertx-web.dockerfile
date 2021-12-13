FROM maven:3.6.1-jdk-11-slim as maven
WORKDIR /vertx-web
COPY scripts scripts
COPY src src
COPY pom.xml pom.xml
RUN mvn package -q

EXPOSE 8080

CMD java \
    -server                                           \
    -XX:+UseNUMA                                      \
    -XX:+UseParallelGC                                \
    -XX:+AggressiveOpts                               \
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
    target/vertx-web-benchmark-4.1.5-fat.jar          \
    --instances                                       \
    `grep --count ^processor /proc/cpuinfo`           \
    --conf                                            \
    src/main/conf/config.json                         \
    --options                                         \
    src/main/conf/vertx.json
