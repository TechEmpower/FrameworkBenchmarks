FROM fedora:40 AS build

WORKDIR /zap

ENV PG_USER=benchmarkdbuser
ENV PG_PASS=benchmarkdbpass
ENV PG_DB=hello_world
ENV PG_HOST=tfb-database
ENV PG_PORT=5432

COPY src src
COPY run.sh run.sh

COPY build.zig.zon build.zig.zon
COPY build.zig build.zig

RUN dnf install -y zig
RUN zig version
# RUN zig build -Doptimize=ReleaseFast 
RUN zig build
RUN cp /zap/zig-out/bin/zap /usr/local/bin

EXPOSE 3000

CMD ["sh", "run.sh"]

# FROM alpine:3.19

# WORKDIR /zap

# ENV PG_USER=benchmarkdbuser
# ENV PG_PASS=benchmarkdbpass
# ENV PG_DB=hello_world
# ENV PG_HOST=tfb-database
# ENV PG_PORT=5432

# RUN apk update
# RUN apk add libc6-compat

# COPY run.sh run.sh

# COPY --from=build /zap/zig-out/bin/zap /usr/local/bin

# EXPOSE 3000

# CMD ["sh", "run.sh"]