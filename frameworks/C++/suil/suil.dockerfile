FROM suilteam/base:alpine

COPY ./ suil-bench

ENV SUIL_VERSION=0.1.1
ENV SUIL_VERSION_TAG=alpha
ENV SUIL_CONFIGURATION=Release

WORKDIR /suil-bench
RUN ./scripts/download-framework.sh

WORKDIR /suil-bench/benchmark
RUN ../scripts/build.sh

ENV POSTGRES_CONN="postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world"

EXPOSE 8080
CMD ["/install/bin/suil-bench"]
