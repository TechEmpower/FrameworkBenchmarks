#!/bin/bash

set -euo pipefail

if [ -f /proc/cpuinfo ]; then
  CPU_COUNT=$(grep --count ^processor /proc/cpuinfo)
else
  CPU_COUNT=$(sysctl -n hw.ncpu 2>/dev/null || echo 1)
fi

JAR_PATH="./build/libs/vertx-web-kotlin-dsljson-benchmark-1.0.0-SNAPSHOT-fat.jar"
PG_DOCKER_PATH="../../../toolset/databases/postgres"

HAS_DB=false
while [[ $# -gt 0 ]]; do
  case $1 in
    -db)
      HAS_DB=true
      ;;
  esac
  shift
done

if [ "$HAS_DB" = true ]; then
  docker image inspect tfb-postgres >/dev/null 2>&1 || \
    docker build -f "$PG_DOCKER_PATH/postgres.dockerfile" -t tfb-postgres "$PG_DOCKER_PATH"

  # ensure no old container blocks name reuse
  docker rm -f tfb-postgres >/dev/null 2>&1 || true

  docker run --rm --name tfb-postgres -p 5432:5432 -d tfb-postgres

  until PGPASSWORD=benchmarkdbpass psql -h "0.0.0.0" -U benchmarkdbuser -d hello_world -c '\dt' > /dev/null 2>&1; do
    sleep 1
  done
fi

cleanup() {
  echo "Caught termination signal. Cleaning up..."
  if [ -n "${JAVA_PID:-}" ]; then
    kill -SIGTERM "$JAVA_PID" 2>/dev/null || true
    wait "$JAVA_PID" 2>/dev/null || true
  fi

  if [ "$HAS_DB" = true ]; then
    echo "Stopping postgres container..."
    docker stop tfb-postgres >/dev/null 2>&1 || true
    docker rm -f tfb-postgres >/dev/null 2>&1 || true
  fi
}

trap cleanup SIGINT SIGTERM EXIT

java \
  -server \
  --enable-native-access=ALL-UNNAMED \
  --add-opens=java.base/java.lang=ALL-UNNAMED \
  -Xms2G \
  -Xmx2G \
  -XX:+AlwaysPreTouch \
  -XX:+UseZGC \
  -XX:+ZUncommit \
  -XX:+DisableExplicitGC \
  -XX:+UseLargePages \
  -XX:+UseStringDeduplication \
  -XX:+EnableDynamicAgentLoading \
  -XX:InitialCodeCacheSize=512m \
  -XX:ReservedCodeCacheSize=512m \
  -XX:MaxInlineLevel=20 \
  -XX:+UseNUMA \
  -XX:-UseCodeCacheFlushing \
  -XX:AutoBoxCacheMax=10001 \
  -Djava.net.preferIPv4Stack=true \
  -Dvertx.disableMetrics=true \
  -Dvertx.disableDnsResolver=true \
  -Dvertx.disableWebsockets=true \
  -Dvertx.disableContextTimings=true \
  -Dvertx.cacheImmutableHttpResponseHeaders=true \
  -Dvertx.internCommonHttpRequestHeadersToLowerCase=true \
  -Dvertx.disableHttpHeadersValidation=true \
  -Dio.netty.noUnsafe=false \
  -Dio.netty.buffer.checkBounds=false \
  -Dio.netty.buffer.checkAccessible=false \
  -Dio.netty.recycler.maxCapacity.default=0 \
  -Dio.netty.maxDirectMemory=0 \
  "-Dtfb.hasDB=$HAS_DB" \
  "-Dtfb.pgHostOverride=0.0.0.0" \
  -jar "$JAR_PATH" &

JAVA_PID=$!

echo "Server PID: $JAVA_PID"

wait "$JAVA_PID"
