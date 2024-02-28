#FROM ziglang/static-base:llvm15-aarch64-3 as build
FROM buddyspencer/ziglang:0.11.0-r3

WORKDIR /zap
COPY src src

COPY build.zig.zon build.zig.zon
COPY build.zig build.zig

RUN apk update
RUN apk add yaml-dev sqlite-dev
RUN zig build -Doptimize=ReleaseFast --prefix-exe-dir /usr/bin
RUN ls /usr/bin

EXPOSE 3000

CMD ["zap"]



