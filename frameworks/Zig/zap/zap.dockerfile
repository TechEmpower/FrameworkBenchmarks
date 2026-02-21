FROM fedora:42 AS build

WORKDIR /zap

ENV PG_USER=benchmarkdbuser
ENV PG_PASS=benchmarkdbpass
ENV PG_DB=hello_world
ENV PG_HOST=tfb-database
ENV PG_PORT=5432

COPY src src
COPY build.zig.zon build.zig.zon
COPY build.zig build.zig
COPY start-servers.sh start-servers.sh
COPY build-nginx-conf.sh build-nginx-conf.sh
COPY nginx.conf nginx.conf

RUN chmod +x start-servers.sh
RUN chmod +x build-nginx-conf.sh

RUN ./build-nginx-conf.sh

RUN dnf install -y nginx tar xz && \
    curl -L https://ziglang.org/download/0.15.1/zig-linux-x86_64-0.15.1.tar.xz -o /tmp/zig.tar.xz && \
    tar xf /tmp/zig.tar.xz -C /usr/local && \
    ln -s /usr/local/zig-linux-x86_64-0.15.1/zig /usr/local/bin/zig && \
    rm /tmp/zig.tar.xz
RUN zig version
RUN zig build -Doptimize=ReleaseFast 
RUN cp /zap/zig-out/bin/zap /usr/local/bin

EXPOSE 8080

CMD ./start-servers.sh && nginx -c /zap/nginx.conf -g "daemon off;"