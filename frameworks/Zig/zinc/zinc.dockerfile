FROM fedora:40 AS build

WORKDIR /zinc

COPY src src
COPY run.sh run.sh

COPY build.zig.zon build.zig.zon
COPY build.zig build.zig

RUN dnf install -y zig
RUN zig version
RUN zig build -Doptimize=ReleaseFast
RUN cp /zinc/zig-out/bin/zinc /usr/local/bin

EXPOSE 3000
ARG BENCHMARK_ENV
ARG TFB_TEST_DATABASE
ARG TFB_TEST_NAME

CMD ["sh", "run.sh"]
