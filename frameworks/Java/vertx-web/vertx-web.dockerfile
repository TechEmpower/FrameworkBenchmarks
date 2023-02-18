FROM maven:3.9.0-eclipse-temurin-17 as maven
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
    target/vertx-web-benchmark-4.3.8-fat.jar          \
    --instances                                       \
    `grep --count ^processor /proc/cpuinfo`           \
    --conf                                            \
    src/main/conf/config.json                         \
    --options                                         \
    src/main/conf/vertx.json
