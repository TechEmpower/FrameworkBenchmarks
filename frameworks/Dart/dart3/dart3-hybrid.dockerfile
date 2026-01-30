
FROM dart:3.10.8 AS build
WORKDIR /app

# Define the build-time argument (Default to 8)
ARG MAX_ISOLATES=8

COPY pubspec.yaml .
COPY bin bin

RUN dart compile exe bin/server.dart \
    --define=MAX_ISOLATES=${MAX_ISOLATES} \
    -o server.aot

FROM traefik:v2.10 AS traefik_source

FROM busybox:glibc
WORKDIR /app
# Define the build-time argument (Default to 8)
ARG MAX_ISOLATES=8

COPY --from=build /runtime/ /
COPY --from=build /app/server /bin/server
COPY --from=traefik_source /usr/local/bin/traefik /usr/local/bin/traefik
COPY /hybrid/traefik.yaml /etc/traefik/traefik.yaml
COPY /hybrid/run.sh /app/run.sh

RUN chmod +x /app/run.sh

EXPOSE 8080
CMD ["/app/run.sh"]