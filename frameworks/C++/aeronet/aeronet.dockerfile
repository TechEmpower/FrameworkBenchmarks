FROM ubuntu:24.04 AS build

ARG AERONET_GIT_TAG=main
ARG BUILD_MODE=Release

RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get install -y --no-install-recommends \
        build-essential \
        ninja-build \
        cmake \
        git \
        ca-certificates && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY main.cpp CMakeLists.txt ./

RUN mkdir build && cd build && \
    cmake -GNinja \
      -DCMAKE_BUILD_TYPE=${BUILD_MODE} \
      -DAERONET_GIT_TAG=${AERONET_GIT_TAG} \
      .. && \
    cmake --build . --target benchmark_techempower && \
    install -m 0755 benchmark_techempower /app/benchmark_techempower

RUN mkdir -p /deps && \
    ldd /app/benchmark_techempower \
    | tr -s '[:blank:]' '\n' \
    | grep '^/' \
    | xargs -I % sh -c 'mkdir -p /deps$(dirname %); cp % /deps%;'

FROM scratch

COPY --from=build /etc/ssl/certs /etc/ssl/certs
COPY --from=build /deps /
COPY --from=build /app/benchmark_techempower /app/benchmark_techempower

EXPOSE 8080

ENTRYPOINT ["/app/benchmark_techempower"]
