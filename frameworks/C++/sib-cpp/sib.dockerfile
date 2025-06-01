FROM ubuntu:24.04

# Install required packages
RUN apt-get update && apt-get install -y \
    git cmake m4 gcc g++ clang-format clang-tidy ninja-build \
    flex bison libunwind-dev libgflags-dev libkrb5-dev \
    libsasl2-dev libnuma-dev pkg-config libssl-dev libcap-dev \
    gperf libevent-dev libtool libboost-all-dev libjemalloc-dev \
    libsnappy-dev wget unzip libiberty-dev liblz4-dev liblzma-dev \
    make zlib1g-dev binutils-dev libsodium-dev \
    libdouble-conversion-dev python3-venv libsecret-1-dev ccache

# Uninstall system-wide fmt/folly to avoid ODR issues
RUN apt-get remove -y libfmt-dev libfolly-dev libfizz-dev libproxygen-dev || true

# Set compiler environment
ENV CC=gcc
ENV CXX=g++
ENV AR=gcc-ar
ENV RANLIB=gcc-ranlib
ENV CMAKE_C_COMPILER_LAUNCHER=ccache
ENV CMAKE_CXX_COMPILER_LAUNCHER=ccache

# Clone SIB with submodules
WORKDIR /sib
RUN git clone --recurse-submodules --depth 1 -b sib-cpp https://github.com/PooyaEimandar/sib.git .

# Copy TechEmpower entry point (override main)
COPY ./main.cpp ./techempower/main.cpp

# Clean build to avoid stale or misconfigured builds
RUN rm -rf build _build CMakeCache.txt

# Build Proxygen + Folly using your CMake
RUN cmake -B build -G Ninja \
    -DCMAKE_BUILD_TYPE=Release \
    -DSIB_TECHEMPOWER=ON \
    -DSIB_NET_PROXYGEN=ON \
    -DCMAKE_C_FLAGS_RELEASE="-O3 -flto -fno-strict-aliasing" \
    -DCMAKE_CXX_FLAGS_RELEASE="-O3 -flto -fno-strict-aliasing" \
    -DCMAKE_EXE_LINKER_FLAGS="-flto" \
 && cmake --build build

EXPOSE 8080
CMD ["./build/sib_techempower"]
