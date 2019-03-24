FROM oracle/graalvm-ce:1.0.0-rc14
# Set working dir
RUN mkdir /app
WORKDIR /app

COPY ./ /app

# Get dependencies
RUN npm --unsafe-perm install

CMD java \
    -server                                             \
    -XX:+UseNUMA                                        \
    -XX:+UseParallelGC                                  \
    -XX:+AggressiveOpts                                 \
    -Dvertx.disableMetrics=true                         \
    -Dvertx.disableH2c=true                             \
    -Dvertx.disableWebsockets=true                      \
    -Dvertx.flashPolicyHandler=false                    \
    -Dvertx.threadChecks=false                          \
    -Dvertx.disableContextTimings=true                  \
    -Dvertx.disableTCCL=true                            \
    -jar node_modules/.bin/benchmark.jar                \
    --instances `grep --count ^processor /proc/cpuinfo` \
    --options vertx.json
