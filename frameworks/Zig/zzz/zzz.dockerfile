FROM debian:12-slim AS build

RUN useradd -m ziguser

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    wget xz-utils \
    ca-certificates && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

ARG ZIG_VER=0.14.0
RUN wget https://ziglang.org/download/${ZIG_VER}/zig-linux-$(uname -m)-${ZIG_VER}.tar.xz

RUN tar -xvf zig-linux-$(uname -m)-${ZIG_VER}.tar.xz

RUN mv zig-linux-$(uname -m)-${ZIG_VER} /usr/local/zig 

ENV PATH="/usr/local/zig:$PATH"

WORKDIR /home/ziguser
COPY src src
COPY build.zig build.zig
COPY build.zig.zon build.zig.zon

USER ziguser

RUN zig build -Doptimize=ReleaseFast -Dcpu=native
RUN ls

FROM debian:12-slim

RUN apt-get -qq update 
RUN apt-get -qy install ca-certificates

COPY --from=build /home/ziguser/zig-out/bin/zzz /server
EXPOSE 8080
ENTRYPOINT ./server
