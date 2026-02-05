
FROM dart:3.10.8 AS build
WORKDIR /app

# Define the maximum number of Dart Isolates at build time
ARG MAX_ISOLATES=10

COPY pubspec.yaml .
COPY bin bin

RUN dart compile exe bin/server.dart \
    --define=MAX_ISOLATES=${MAX_ISOLATES} \
    -o server

FROM traefik:latest AS traefik_source

FROM busybox:glibc
WORKDIR /app
# Matches `ARG MAX_ISOLATES` in `build`
ENV MAX_ISOLATES=10
# Define the minimum number of processes dedicated to `Traefik`
ENV MIN_TRAEFIK_PROCESSES=4

COPY --from=build /runtime/ /
COPY --from=build /app/server /app/server
COPY --from=traefik_source /usr/local/bin/traefik /usr/local/bin/traefik
COPY /dart_hybrid/traefik.yaml /etc/traefik/traefik.yaml
COPY /dart_hybrid/traefik_dynamic.yaml /etc/traefik/traefik_dynamic.yaml
COPY /dart_hybrid/run.sh /app/run.sh

RUN chmod +x /app/run.sh

EXPOSE 8080
CMD ["/app/run.sh"]