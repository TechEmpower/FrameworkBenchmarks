#!/bin/bash

sed -i 's|localhost|'"${DBHOST}"'|g' src/main/conf/config.json

fw_depends java maven

mvn clean package

java \
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
  target/vertx-web-benchmark-3.5.0-fat.jar    \
  --instances                                       \
  `grep --count ^processor /proc/cpuinfo`           \
  --conf                                            \
  src/main/conf/config.json &
