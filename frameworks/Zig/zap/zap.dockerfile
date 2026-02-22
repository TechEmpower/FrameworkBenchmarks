FROM fedora:42

WORKDIR /zap

ENV PG_USER=benchmarkdbuser
ENV PG_PASS=benchmarkdbpass
ENV PG_DB=hello_world
ENV PG_HOST=tfb-database
ENV PG_PORT=5432

COPY src src
COPY build.zig.zon build.zig.zon
COPY build.zig build.zig

RUN dnf install -y tar xz git wget nginx

ARG ZIG_VER=0.15.2

RUN wget https://ziglang.org/download/${ZIG_VER}/zig-$(uname -m)-linux-${ZIG_VER}.tar.xz

RUN tar -xvf zig-$(uname -m)-linux-${ZIG_VER}.tar.xz

RUN mv zig-$(uname -m)-linux-${ZIG_VER} /usr/local/zig

ENV PATH="/usr/local/zig:$PATH"
COPY start-servers.sh start-servers.sh
COPY build-nginx-conf.sh build-nginx-conf.sh
COPY nginx.conf nginx.conf

RUN chmod +x start-servers.sh build-nginx-conf.sh && ./build-nginx-conf.sh

RUN zig version
RUN zig build -Doptimize=ReleaseFast
RUN cp /zap/zig-out/bin/zap /usr/local/bin

EXPOSE 8080

CMD ./start-servers.sh && nginx -c /zap/nginx.conf -g "daemon off;"
