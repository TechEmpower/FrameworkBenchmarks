FROM maven:3.5.3-jdk-10-slim as maven
WORKDIR /vertx-web
COPY scripts scripts
COPY src src
COPY pom.xml pom.xml
RUN mvn package -q
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
    -jar                                              \
    target/vertx-web-benchmark-3.5.0-fat.jar          \
    --instances                                       \
    `grep --count ^processor /proc/cpuinfo`           \
    --conf                                            \
    src/main/conf/config.json
