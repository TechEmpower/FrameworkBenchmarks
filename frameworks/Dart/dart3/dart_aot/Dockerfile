
FROM dart:3.11.0 AS build
WORKDIR /app

ARG MAX_ISOLATES=8

COPY pubspec.yaml .
COPY bin bin

RUN dart compile aot-snapshot bin/server.dart \
    --define=MAX_ISOLATES=${MAX_ISOLATES} \
    -o server.aot

FROM gcr.io/distroless/base-debian12
WORKDIR /app

COPY --from=build /usr/lib/dart/bin/dartaotruntime /usr/lib/dart/bin/dartaotruntime
COPY --from=build /app/server.aot /app/server.aot

EXPOSE 8080

ENTRYPOINT ["/usr/lib/dart/bin/dartaotruntime", "/app/server.aot"]
