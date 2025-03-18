FROM debian:12.9

ENV PG_USER=benchmarkdbuser
ENV PG_PASS=benchmarkdbpass
ENV PG_DB=hello_world
ENV PG_HOST=tfb-database
ENV PG_PORT=5432

WORKDIR /app

COPY src src
COPY build.zig.zon build.zig.zon
COPY build.zig build.zig

ARG ZIG_VER=0.13.0

RUN apt-get update && apt-get install -y curl xz-utils ca-certificates

RUN curl https://ziglang.org/download/${ZIG_VER}/zig-linux-$(uname -m)-${ZIG_VER}.tar.xz -o zig-linux.tar.xz && \
    tar xf zig-linux.tar.xz && \
    mv zig-linux-$(uname -m)-${ZIG_VER}/ /opt/zig

RUN /opt/zig/zig build -Doptimize=ReleaseFast

EXPOSE 3000
RUN ls 

CMD ["zig-out/bin/httpz"]
