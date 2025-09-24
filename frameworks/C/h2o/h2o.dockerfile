ARG UBUNTU_VERSION=25.10

ARG H2O_APP_PREFIX=/opt/h2o-app

FROM "ubuntu:${UBUNTU_VERSION}" AS compile

RUN echo "[timing] Installing system packages: $(date)"
ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get -yqq update && \
    apt-get -yqq install \
      autoconf \
      bison \
      clang \
      cmake \
      curl \
      flex \
      libbrotli-dev \
      libcap-dev \
      libnuma-dev \
      libpq-dev \
      libssl-dev \
      libstdc++-15-dev \
      libtool \
      liburing-dev \
      libuv1-dev \
      libwslay-dev \
      libyajl-dev \
      libz-dev \
      make \
      ninja-build \
      pkg-config \
      ruby \
      systemtap-sdt-dev

RUN echo "[timing] Building H2O: $(date)"
ARG H2O_VERSION=3b9b6a53cac8bcc6a25fb28df81ad295fc5f9402

WORKDIR /tmp/h2o-build
RUN curl -LSs "https://github.com/h2o/h2o/archive/${H2O_VERSION}.tar.gz" | \
      tar --strip-components=1 -xz && \
    cmake \
      -B build \
      -DCMAKE_AR=/usr/bin/gcc-ar \
      -DCMAKE_C_FLAGS="-flto=auto -march=native -mtune=native" \
      -DCMAKE_RANLIB=/usr/bin/gcc-ranlib \
      -DWITH_MRUBY=on \
      -G Ninja \
      -S . && \
    cmake --build build -j && \
    cmake --install build

RUN echo "[timing] Building mustache-c: $(date)"
ARG MUSTACHE_C_REVISION=7fe52392879d0188c172d94bb4fde7c513d6b929

WORKDIR /tmp/mustache-c-build
RUN curl -LSs "https://github.com/x86-64/mustache-c/archive/${MUSTACHE_C_REVISION}.tar.gz" | \
      tar --strip-components=1 -xz && \
    CFLAGS="-flto=auto -march=native -mtune=native -O3 -Wno-implicit-function-declaration" \
      ./autogen.sh && \
    make -j "$(nproc)" install

RUN echo "[timing] Building h2o-app: $(date)"
ARG H2O_APP_PREFIX
WORKDIR /tmp/build
COPY CMakeLists.txt ../
COPY src ../src/
COPY template ../template/
RUN cmake \
      -DCMAKE_BUILD_TYPE=Release \
      -DCMAKE_C_FLAGS="-march=native -mtune=native" \
      -DCMAKE_INSTALL_PREFIX="${H2O_APP_PREFIX}" \
      -G Ninja \
      -S .. && \
    cmake --build . -j && \
    cmake --install .
RUN echo "[timing] Finished compiling: $(date)"

FROM "ubuntu:${UBUNTU_VERSION}"

RUN echo "[timing] Installing final system packages: $(date)"
ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get -yqq update && \
    apt-get -yqq install \
      libnuma1 \
      libpq5 \
      liburing2 \
      libyajl2
RUN echo "[timing] Copying h2o-app to its final location: $(date)"
ARG H2O_APP_PREFIX
COPY --from=compile "${H2O_APP_PREFIX}" "${H2O_APP_PREFIX}/"
COPY --from=compile /usr/local/lib/libmustache_c.so "${H2O_APP_PREFIX}/lib/"
ENV LD_LIBRARY_PATH="${H2O_APP_PREFIX}/lib"
EXPOSE 8080
ARG BENCHMARK_ENV
ARG TFB_TEST_DATABASE
ARG TFB_TEST_NAME
RUN echo "[timing] Running h2o-app: $(date)"

CMD ["taskset", \
     "-c", \
     "0", \
     "/opt/h2o-app/bin/h2o-app", \
     "-a20", \
     "-d", \
     "dbname=hello_world host=tfb-database password=benchmarkdbpass sslmode=disable user=benchmarkdbuser", \
     "-f", \
     "/opt/h2o-app/share/h2o-app/template", \
     "-m1"]
