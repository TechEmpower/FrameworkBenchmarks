#FROM ziglang/static-base:llvm15-aarch64-3 as build
FROM buddyspencer/ziglang:0.11.0-r3 as build

WORKDIR /zap

COPY src src

COPY build.zig.zon build.zig.zon
COPY build.zig build.zig

RUN apk update
RUN apk add yaml-dev sqlite-dev
RUN apk add bind-tools
RUN apk add --no-cache bash
RUN dig +short localhost | head -n 1
RUN zig build -Doptimize=ReleaseFast --prefix-exe-dir /usr/bin
RUN zig version
RUN ls

EXPOSE 3000

CMD ["sh", "run.sh"]

FROM alpine:3.19

WORKDIR /zap

ENV PG_USER=benchmarkdbuser
ENV PG_PASS=benchmarkdbpass
ENV PG_DB=hello_world
ENV PG_HOST=tfb-database
ENV PG_PORT=5432

COPY run.sh run.sh

RUN apk update

COPY --from=build /usr/bin/zap /usr/bin/zap

EXPOSE 3000

CMD ["sh", "run.sh"]