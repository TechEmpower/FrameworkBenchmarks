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

ARG ZIG_VER=0.15.2

RUN apt-get update && apt-get install -y wget xz-utils ca-certificates

RUN wget https://ziglang.org/download/${ZIG_VER}/zig-$(uname -m)-linux-${ZIG_VER}.tar.xz

RUN tar -xvf zig-$(uname -m)-linux-${ZIG_VER}.tar.xz

RUN mv zig-$(uname -m)-linux-${ZIG_VER} /usr/local/zig 

ENV PATH="/usr/local/zig:$PATH"
RUN zig build -Doptimize=ReleaseFast

EXPOSE 3000

CMD ["zig-out/bin/dusty"]
