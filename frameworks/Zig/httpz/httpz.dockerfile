FROM eloitor/zig:0.13.0 AS build

ARG ZIG_VER=0.13

WORKDIR /app

COPY . .

RUN zig build -Doptimize=ReleaseFast

FROM debian:12.9-slim

RUN apt-get update && apt-get install -y ca-certificates

COPY --from=build /app/zig-out/bin/httpz /server

CMD ["/server"]
