FROM eloitor/zig:0.13.0 AS build

ENV PG_USER=benchmarkdbuser
ENV PG_PASS=benchmarkdbpass
ENV PG_DB=hello_world
ENV PG_HOST=tfb-database
ENV PG_PORT=5432

WORKDIR /app

COPY src src
COPY build.zig.zon build.zig.zon
COPY build.zig build.zig

RUN zig build -Doptimize=ReleaseFast

FROM debian:12.9-slim

RUN apt-get update && apt-get install -y ca-certificates

COPY --from=build /app/zig-out/bin/httpz /server

EXPOSE 3000

CMD ["/server"]
