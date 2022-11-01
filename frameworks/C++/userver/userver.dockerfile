FROM ghcr.io/userver-framework/docker-userver-build-base:v1a
WORKDIR src
COPY userver_benchmark/src/ ./
RUN git clone https://github.com/userver-framework/userver.git && \
    cd userver && git checkout 84456123a6a32f68847a791f29ef56d86bb765ad
RUN mkdir build && cd build && \
    cmake -DUSERVER_IS_THE_ROOT_PROJECT=0 -DUSERVER_OPEN_SOURCE_BUILD=1 -DUSERVER_FEATURE_CRYPTOPP_BLAKE2=0 \
          -DUSERVER_FEATURE_REDIS=0 -DUSERVER_FEATURE_CLICKHOUSE=0 -DUSERVER_FEATURE_MONGODB=0 -DUSERVER_FEATURE_RABBITMQ=0 -DUSERVER_FEATURE_GRPC=0 \
          -DUSERVER_FEATURE_UTEST=0 \
          -DUSERVER_FEATURE_POSTGRESQL=1 \
          -DCMAKE_BUILD_TYPE=Release -DCMAKE_CXX_FLAGS="-march=native" .. && \
    make -j $(nproc)

COPY userver_benchmark/configs/* ./

EXPOSE 8090
CMD ./build/userver_techempower -c /src/static_config.yaml
