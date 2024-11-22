FROM fedora:40

WORKDIR /httpz

ENV PG_USER=benchmarkdbuser
ENV PG_PASS=benchmarkdbpass
ENV PG_DB=hello_world
ENV PG_HOST=tfb-database
ENV PG_PORT=5432

COPY src src
COPY build.zig.zon build.zig.zon
COPY build.zig build.zig
COPY run.sh run.sh

RUN dnf install -y zig
RUN zig version
RUN zig build -Doptimize=ReleaseFast 
RUN cp /httpz/zig-out/bin/httpz /usr/local/bin

EXPOSE 3000

CMD ["sh", "run.sh"]