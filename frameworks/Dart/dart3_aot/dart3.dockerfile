# ---------- Build stage ----------
FROM dart:stable AS build

WORKDIR /app

COPY pubspec.yaml .
RUN dart pub get

COPY bin bin

RUN dart compile aot-snapshot bin/server.dart -o server.aot


# ---------- Runtime stage ----------
FROM gcr.io/distroless/base-debian12

# Copy Dart AOT runtime
COPY --from=build /usr/lib/dart/bin/dartaotruntime /usr/lib/dart/bin/dartaotruntime

# Copy snapshot
COPY --from=build /app/server.aot /app/server.aot

WORKDIR /app

EXPOSE 8080

# Distroless requires absolute paths
ENTRYPOINT ["/usr/lib/dart/bin/dartaotruntime", "/app/server.aot"]
