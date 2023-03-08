FROM ghcr.io/userver-framework/docker-userver-build-base:v1a AS builder
WORKDIR /src
RUN git clone https://github.com/userver-framework/userver.git && \
    cd userver && git checkout 151bc1e3df01807da9cd27f9677b80f4951b1f25
COPY userver_benchmark/ ./
RUN mkdir build && cd build && \
    cmake -DUSERVER_IS_THE_ROOT_PROJECT=0 -DUSERVER_OPEN_SOURCE_BUILD=1 -DUSERVER_FEATURE_CRYPTOPP_BLAKE2=0 \
          -DUSERVER_FEATURE_REDIS=0 -DUSERVER_FEATURE_CLICKHOUSE=0 -DUSERVER_FEATURE_MONGODB=0 -DUSERVER_FEATURE_RABBITMQ=0 -DUSERVER_FEATURE_GRPC=0 \
          -DUSERVER_FEATURE_UTEST=0 \
          -DUSERVER_FEATURE_POSTGRESQL=1 \
          -DCMAKE_BUILD_TYPE=Release -DCMAKE_CXX_FLAGS="-march=native" .. && \
    make -j $(nproc)

FROM builder AS runner
WORKDIR /app
COPY userver_configs/* ./
COPY --from=builder /src/build/userver_techempower ./

EXPOSE 8081
CMD ./userver_techempower -c ./static_config.yaml

