FROM ghcr.io/userver-framework/ubuntu-userver-build-base:v2 AS builder

RUN apt update && \
    apt install -y lsb-release wget software-properties-common gnupg && \
        wget https://apt.llvm.org/llvm.sh && chmod +x llvm.sh && ./llvm.sh 16

WORKDIR /src
RUN git clone https://github.com/userver-framework/userver.git && \
    cd userver && git checkout c2ca5454f0b0e93dd0a2e082904dedda5cda3052

COPY userver_benchmark/ ./
RUN mkdir build && cd build && \
    cmake -DUSERVER_IS_THE_ROOT_PROJECT=0 -DUSERVER_FEATURE_CRYPTOPP_BLAKE2=0 \
          -DUSERVER_FEATURE_UTEST=0 \
          -DUSERVER_FEATURE_POSTGRESQL=1 \
          -DUSERVER_FEATURE_ERASE_LOG_WITH_LEVEL=warning \
          -DCMAKE_BUILD_TYPE=Release -DCMAKE_CXX_FLAGS="-march=native -flto=thin" -DCMAKE_C_FLAGS="-march=native -flto=thin" \
          -DCMAKE_CXX_COMPILER=clang++-16 -DCMAKE_C_COMPILER=clang-16 -DUSERVER_USE_LD=lld-16 \
          -DUSERVER_LTO=0 .. && \
    make -j $(nproc)

FROM builder AS runner
WORKDIR /app
COPY userver_configs/* ./
COPY --from=builder /src/build/userver_techempower ./

EXPOSE 8080
CMD ./userver_techempower -c ./static_config.yaml

