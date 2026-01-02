
FROM dart:3.10.7 AS builder
WORKDIR /app

# Define the build-time argument (Default to 8)
ARG MAX_ISOLATES=8
COPY . .
RUN mkdir build
RUN dart compile exe bin/server.dart \
    --define=MAX_ISOLATES=${MAX_ISOLATES} \
    -o build/server

FROM busybox:glibc

# Re-declare ARG in the second stage to use it in ENV
# Define the build-time argument (Default to 8)
ARG MAX_ISOLATES=8
ENV MAX_ISOLATES_PER_PROCESS=${MAX_ISOLATES}

COPY --from=builder /runtime/ /
COPY --from=builder /app/build/server /bin/server
COPY run.sh /bin/run.sh
RUN chmod +x /bin/run.sh

EXPOSE 8080
CMD ["/bin/run.sh"]