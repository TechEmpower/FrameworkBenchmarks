FROM ubuntu:16.04

COPY ./ ./

RUN apt update && \
    apt install -yqq autoconf bison cmake curl file flex g++ git libnuma-dev libpq-dev libssl-dev \
                     libtool libyajl-dev make wget

### Install mustache-c

ENV MUSTACHE_C_BUILD_DIR=mustache-c
ENV MUSTACHE_C_HOME=/mustache-c

RUN git clone "https://github.com/x86-64/mustache-c.git" "$MUSTACHE_C_BUILD_DIR" && \
    cd "$MUSTACHE_C_BUILD_DIR" && \
    git checkout 01f1e4732c4862071bbf07242128abf1e28cc105 && \
    CFLAGS="-O3 -flto -march=native" ./autogen.sh --prefix="$MUSTACHE_C_HOME" && \
    make -j "$(nproc)" install

ENV LD_LIBRARY_PATH="${MUSTACHE_C_HOME}/lib:$LD_LIBRARY_PATH"

### Install h2o

ENV H2O_VERSION=2.2.4
ENV H2O_ARCHIVE="v${H2O_VERSION}.tar.gz"
ENV H2O_HOME=/h2o

RUN wget -qO "$H2O_ARCHIVE" "https://github.com/h2o/h2o/archive/$H2O_ARCHIVE" && \
    tar xf "$H2O_ARCHIVE" && \
    cd "h2o-$H2O_VERSION" && \
    cmake -DCMAKE_INSTALL_PREFIX="$H2O_HOME" -DCMAKE_C_FLAGS="-flto -march=native" \
          -DCMAKE_AR=/usr/bin/gcc-ar -DCMAKE_RANLIB=/usr/bin/gcc-ranlib && \
    make -j "$(nproc)" install

CMD ["./start-servers.sh"]
