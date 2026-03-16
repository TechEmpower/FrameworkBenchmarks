FROM ubuntu:24.04 AS build
RUN apt-get update && apt-get install -y --no-install-recommends \
    g++-13 git ca-certificates \
    libpq-dev \
    && rm -rf /var/lib/apt/lists/*
ENV CXX=g++-13
ENV CC=gcc-13
WORKDIR /app
RUN git clone --depth 1 https://github.com/olafurjohannsson/lundicpp.git /lundi
RUN git clone --depth 1 --branch asio-1-30-2 \
    https://github.com/chriskohlhoff/asio.git /asio
COPY src/ src/
RUN g++-13 -std=c++20 -O3 -march=native -mtune=native -flto \
    -I/lundi/include \
    -I/asio/asio/include \
    -I/usr/include/postgresql \
    -DASIO_STANDALONE -DASIO_NO_DEPRECATED \
    -o tfb_bench src/tfb_main.cpp \
    -lpq -lpthread

FROM ubuntu:24.04
RUN apt-get update && apt-get install -y --no-install-recommends \
    libpq5 && rm -rf /var/lib/apt/lists/*
COPY --from=build /app/tfb_bench /tfb_bench
EXPOSE 8080
CMD ["/tfb_bench"]