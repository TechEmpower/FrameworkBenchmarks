FROM ghcr.io/userver-framework/ubuntu-userver-build-base:v1 AS builder

WORKDIR /src
RUN git clone https://github.com/userver-framework/userver.git && \
    cd userver && git checkout 040b0bd001a078ee023aabe987e5ec7cc8ed5b6c
COPY userver_benchmark/ ./
RUN mkdir build && cd build && \
    cmake -DUSERVER_IS_THE_ROOT_PROJECT=0 -DUSERVER_FEATURE_CRYPTOPP_BLAKE2=0 \
          -DUSERVER_FEATURE_REDIS=0 -DUSERVER_FEATURE_CLICKHOUSE=0 -DUSERVER_FEATURE_MONGODB=0 -DUSERVER_FEATURE_RABBITMQ=0 -DUSERVER_FEATURE_GRPC=0 \
          -DUSERVER_FEATURE_UTEST=0 \
          -DUSERVER_FEATURE_POSTGRESQL=1 \
          -DCMAKE_BUILD_TYPE=Release -DCMAKE_CXX_FLAGS="-march=native" .. && \
    make -j $(nproc)

FROM builder AS runner
WORKDIR /app
COPY userver_configs/* ./
COPY --from=builder /src/build/userver_techempower ./

EXPOSE 8080
CMD ./userver_techempower -c ./static_config.yaml

